{-# LANGUAGE OverloadedStrings #-}

-- | High-level entry points that stitch parsing, checking, compilation, ambient
-- declaration discovery, and emission together.
--
-- The rest of the repo treats this module as the main service layer. CLI
-- commands call it directly, the LSP keeps analyzed results from it in memory,
-- and tests use it to exercise end-to-end behavior.
module Driver
  ( Analysis (..),
    analyzeFile,
    analyzeText,
    compileFile,
    compileText,
    emitFile,
    emitText,
    lookupSymbolType,
    parseText,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM)
import Data.List (group, isSuffixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), isAbsolute, joinPath, normalise, splitDirectories, takeDirectory)
import Alias
import Check
import Compile
import Emit
import Indexed
import Kind
import Parser
import Syntax
import Type

-- | End-to-end analysis result for one file.
--
-- Keeping the parsed program alongside inferred schemes lets downstream tools
-- answer both syntactic and semantic questions without reparsing.
data Analysis = Analysis
  { analysisProgram :: Program,
    analysisRoot :: Maybe Scheme,
    analysisBindings :: Map Name Scheme,
    analysisAliases :: AliasEnv,
    analysisAmbient :: Map FilePath Scheme
  }
  deriving (Eq, Show)

-- | Parse a source buffer and normalize parser errors to plain strings.
parseText :: FilePath -> Text -> Either String Program
parseText path = either (Left . Text.unpack) Right . parseProgram path

-- | Analyze an in-memory source buffer, loading nearby declaration support.
analyzeText :: FilePath -> Text -> IO (Either String Analysis)
analyzeText path input = do
  support <- loadSupport path
  pure $ do
    supportWorld <- support
    program <- parseText path input
    _ <- validateProgramKinds (programAliases program <> worldAliases supportWorld) program
    _ <- validateProgramIndexedTypes program
    localAmbient <- collectAmbient path program
    let aliases = mkAliasEnv (programAliases program <> worldAliases supportWorld)
        ambient = localAmbient <> worldAmbient supportWorld
        context = CheckContext {checkAliases = aliases, checkAmbient = ambient, checkFile = path}
    result <- checkProgram context program
    pure
      Analysis
        { analysisProgram = program,
          analysisRoot = resultRoot result,
          analysisBindings = resultBindings result,
          analysisAliases = aliases,
          analysisAmbient = ambient
        }

-- | Read and analyze a file from disk.
analyzeFile :: FilePath -> IO (Either String Analysis)
analyzeFile path = Text.readFile path >>= analyzeText path

-- | Compile an in-memory `.tnix` buffer into `.nix` text.
compileText :: FilePath -> Text -> IO (Either String Text)
compileText path input = do
  checked <- analyzeText path input
  pure $ checked >>= \analysis -> either (Left . Text.unpack) Right (compileProgram (analysisProgram analysis))

-- | Compile a file from disk.
compileFile :: FilePath -> IO (Either String Text)
compileFile path = Text.readFile path >>= compileText path

-- | Emit a declaration file for an in-memory source buffer.
emitText :: FilePath -> Text -> IO (Either String Text)
emitText path input = do
  checked <- analyzeText path input
  pure $ do
    analysis <- checked
    root <- maybe (Left "cannot emit declarations from a declaration-only file") Right (analysisRoot analysis)
    pure (emitDeclarationFile path (analysisProgram analysis) root)

-- | Emit a declaration file for a source file on disk.
emitFile :: FilePath -> IO (Either String Text)
emitFile path = Text.readFile path >>= emitText path

-- | Look up a top-level symbol type exposed by an analysis result.
--
-- `default` is synthesized from the root expression so editor tooling can show
-- something useful even when the file does not bind a name explicitly.
lookupSymbolType :: Analysis -> Name -> Maybe Scheme
lookupSymbolType analysis name =
  Map.lookup name (analysisBindings analysis)
    <|> (analysisRoot analysis >>= \root -> if name == "default" then Just root else Nothing)

data World = World
  { worldAliases :: [TypeAlias],
    worldAmbient :: Map FilePath Scheme
  }

builtinsAmbientKey :: FilePath
builtinsAmbientKey = "builtins"

loadSupport :: FilePath -> IO (Either String World)
loadSupport path = do
  root <- findSupportRoot path
  exists <- doesDirectoryExist root
  if not exists
    then pure (Right (World [] Map.empty))
    else do
      files <- sort . filter (/= normalise path) <$> findDeclarationFiles root
      worlds <- forM files loadDeclarationFile
      pure $ do
        loaded <- sequence worlds
        ambient <- mergeAmbientWorlds (zip files (map worldAmbient loaded))
        pure World {worldAliases = concatMap worldAliases loaded, worldAmbient = ambient}

loadDeclarationFile :: FilePath -> IO (Either String World)
loadDeclarationFile path = do
  input <- Text.readFile path
  pure $ do
    program <- firstError ("failed to load declaration file " <> path <> ": ") (parseText path input)
    case markedValue <$> programExpr program of
      Just _ -> Left ("declaration files must not contain executable expressions: " <> path)
      Nothing -> do
        _ <- validateProgramKinds (programAliases program) program
        _ <- validateProgramIndexedTypes program
        ambient <- collectAmbient path program
        pure World {worldAliases = programAliases program, worldAmbient = ambient}

collectAmbient :: FilePath -> Program -> Either String (Map FilePath Scheme)
collectAmbient file program = do
  let duplicates = duplicateNames (map (resolvePath file . ambientPath) (programAmbient program))
  case duplicates of
    dup : _ -> Left ("duplicate ambient declarations for target " <> show dup <> " in " <> file)
    [] -> Map.fromList <$> traverse toPair (programAmbient program)
  where
    toPair decl = do
      scheme <- schemeFromEntries (ambientEntries decl)
      pure (resolvePath file (ambientPath decl), scheme)
    schemeFromEntries entries =
      case duplicateNames (map ambientEntryName entries) of
        dup : _ -> Left ("duplicate ambient entry " <> show dup <> " in " <> file)
        [] ->
          Right $
            case entries of
              [AmbientEntry "default" ty] -> schemeFromAnnotation ty
              _ -> Scheme [] (TRecord (Map.fromList [(ambientEntryName entry, ambientEntryType entry) | entry <- entries]))

findDeclarationFiles :: FilePath -> IO [FilePath]
findDeclarationFiles dir = do
  names <- sort <$> listDirectory dir
  fmap concat $
    forM names $ \name -> do
      let path = dir </> name
      isDir <- doesDirectoryExist path
      if isDir
        then findDeclarationFiles path
        else pure [normalise path | ".d.tnix" `isSuffixOf` name]

resolvePath :: FilePath -> FilePath -> FilePath
resolvePath _ "builtins" = builtinsAmbientKey
resolvePath from target
  | isAbsolute target = collapseParentSegments target
  | otherwise = collapseParentSegments (takeDirectory from </> target)

findSupportRoot :: FilePath -> IO FilePath
findSupportRoot path = go start
  where
    start = normalise (takeDirectory path)
    go dir = do
      marked <- hasWorkspaceMarker dir
      let parent = normalise (takeDirectory dir)
      if marked
        then pure dir
        else
          if parent == dir
            then pure start
            else go parent

hasWorkspaceMarker :: FilePath -> IO Bool
hasWorkspaceMarker dir =
  or
    <$> sequence
      [ doesFileExist (dir </> "flake.nix"),
        doesFileExist (dir </> "cabal.project"),
        doesFileExist (dir </> "pnpm-workspace.yaml"),
        doesDirectoryExist (dir </> ".git")
      ]

mergeAmbientWorlds :: [(FilePath, Map FilePath Scheme)] -> Either String (Map FilePath Scheme)
mergeAmbientWorlds = fmap snd . foldM step (Map.empty, Map.empty)
  where
    step (sources, ambient) (file, additions) =
      foldM (insertOne file) (sources, ambient) (Map.toList additions)
    insertOne file (sources, ambient) (target, scheme) =
      case Map.lookup target sources of
        Just firstSource ->
          Left
            ( "duplicate ambient declarations for target "
                <> show target
                <> " in "
                <> firstSource
                <> " and "
                <> file
            )
        Nothing ->
          Right (Map.insert target file sources, Map.insert target scheme ambient)

duplicateNames :: Ord a => [a] -> [a]
duplicateNames = foldr step [] . group . sort
  where
    step xs acc =
      case xs of
        first : _ | length xs > 1 -> first : acc
        _ -> acc

firstError :: String -> Either String a -> Either String a
firstError prefix = either (Left . (prefix <>)) Right

collapseParentSegments :: FilePath -> FilePath
collapseParentSegments = joinPath . foldl step [] . splitDirectories . normalise
  where
    step acc "." = acc
    step [root] ".." | isAbsoluteRoot root = [root]
    step [] ".." = [".."]
    step acc ".." =
      case reverse acc of
        [] -> [".."]
        root : rest
          | isAbsoluteRoot root -> reverse (root : rest)
        _ : rest -> reverse rest
    step acc part = acc <> [part]
    isAbsoluteRoot part = part == "/"
