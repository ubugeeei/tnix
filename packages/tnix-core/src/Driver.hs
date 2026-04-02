{-# LANGUAGE LambdaCase #-}
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
    emitFileAs,
    emitFile,
    emitFileTo,
    emitText,
    emitTextAs,
    emitTextTo,
    lookupSymbolType,
    parseText,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Control.Monad (foldM, forM)
import Data.List (group, isSuffixOf, nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), isAbsolute, joinPath, normalise, replaceExtension, splitDirectories, takeDirectory)
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
analyzeFile path = readTextFile path >>= either (pure . Left) (analyzeText path)

-- | Compile an in-memory `.tnix` buffer into `.nix` text.
compileText :: FilePath -> Text -> IO (Either String Text)
compileText path input = do
  checked <- analyzeText path input
  pure $ checked >>= \analysis -> either (Left . Text.unpack) Right (compileProgram (analysisProgram analysis))

-- | Compile a file from disk.
compileFile :: FilePath -> IO (Either String Text)
compileFile path = readTextFile path >>= either (pure . Left) (compileText path)

-- | Emit a declaration file for an in-memory source buffer.
emitText :: FilePath -> Text -> IO (Either String Text)
emitText path input = do
  checked <- analyzeText path input
  pure $ do
    analysis <- checked
    root <- maybe (Left "cannot emit declarations from a declaration-only file") Right (analysisRoot analysis)
    pure (emitDeclarationFile path (analysisProgram analysis) root)

emitTextTo :: FilePath -> FilePath -> Text -> IO (Either String Text)
emitTextTo source declarationPath input = do
  let runtimeTarget = replaceExtension source "nix"
  emitTextAs source runtimeTarget declarationPath input

emitTextAs :: FilePath -> FilePath -> FilePath -> Text -> IO (Either String Text)
emitTextAs source runtimeTarget declarationPath input = do
  checked <- analyzeText source input
  pure $ do
    analysis <- checked
    root <- maybe (Left "cannot emit declarations from a declaration-only file") Right (analysisRoot analysis)
    pure (emitDeclarationFileFor runtimeTarget declarationPath (analysisProgram analysis) root)

-- | Emit a declaration file for a source file on disk.
emitFile :: FilePath -> IO (Either String Text)
emitFile path = readTextFile path >>= either (pure . Left) (emitText path)

emitFileTo :: FilePath -> FilePath -> IO (Either String Text)
emitFileTo source declarationPath = readTextFile source >>= either (pure . Left) (emitTextTo source declarationPath)

emitFileAs :: FilePath -> FilePath -> FilePath -> IO (Either String Text)
emitFileAs source runtimeTarget declarationPath = readTextFile source >>= either (pure . Left) (emitTextAs source runtimeTarget declarationPath)

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

data DeclarationSupportFile = DeclarationSupportFile
  { declarationLoadPath :: FilePath,
    declarationResolvePath :: FilePath
  }
  deriving (Eq, Ord, Show)

builtinsAmbientKey :: FilePath
builtinsAmbientKey = "builtins"

loadSupport :: FilePath -> IO (Either String World)
loadSupport path = do
  root <- findSupportRoot path
  exists <- doesDirectoryExist root
  if not exists
    then pure (Right (World [] Map.empty))
    else do
      workspaceFiles <- map (\file -> DeclarationSupportFile file file) <$> findWorkspaceDeclarationFiles root
      configuredFilesResult <- loadConfiguredDeclarationFiles root
      case configuredFilesResult of
        Left err -> pure (Left err)
        Right configuredFiles -> do
          let declarationFiles =
                dedupeDeclarationFiles path (workspaceFiles <> configuredFiles)
          worlds <- traverse loadDeclarationFile declarationFiles
          pure $ do
            loaded <- sequence worlds
            mergeLoadedWorlds declarationFiles loaded

dedupeDeclarationFiles :: FilePath -> [DeclarationSupportFile] -> [DeclarationSupportFile]
dedupeDeclarationFiles source =
  nub
    . sort
    . filter ((/= normalise source) . declarationLoadPath)
    . map normalizeDeclarationSupportFile

normalizeDeclarationSupportFile :: DeclarationSupportFile -> DeclarationSupportFile
normalizeDeclarationSupportFile file =
  file
    { declarationLoadPath = normalise (declarationLoadPath file),
      declarationResolvePath = normalise (declarationResolvePath file)
    }

loadConfiguredDeclarationFiles :: FilePath -> IO (Either String [DeclarationSupportFile])
loadConfiguredDeclarationFiles root = do
  let configPath = root </> "tnix.config.tnix"
  exists <- doesFileExist configPath
  if not exists
    then pure (Right [])
    else do
      inputResult <- readTextFile configPath
      case inputResult of
        Left err -> pure (Left err)
        Right input ->
          case parseText configPath input of
            Left err -> pure (Left ("failed to parse " <> configPath <> ": " <> err))
            Right program ->
              case configuredDeclarationPackPaths configPath program of
                Left err -> pure (Left err)
                Right packs -> do
                  expanded <- traverse (expandDeclarationPackPath root) packs
                  pure (fmap concat (sequence expanded))

configuredDeclarationPackPaths :: FilePath -> Program -> Either String [FilePath]
configuredDeclarationPackPaths configPath program = do
  expr <- maybe (Left ("tnix.config.tnix must contain a root attribute set: " <> configPath)) (Right . markedValue) (programExpr program)
  decodeDeclarationPackField (takeDirectory configPath) expr

decodeDeclarationPackField :: FilePath -> Expr -> Either String [FilePath]
decodeDeclarationPackField root = \case
  EAttrSet items ->
    case [expr | AttrField "declarationPacks" expr <- items] of
      [] -> Right []
      [expr] -> decodePathList root "declarationPacks" expr
      _ -> Left "duplicate config field: declarationPacks"
  _ -> Left "tnix.config.tnix must evaluate to an attrset"

decodePathList :: FilePath -> Text -> Expr -> Either String [FilePath]
decodePathList root label = \case
  EList items -> traverse decodeItem items
  other -> Left ("expected list of path-like values for " <> Text.unpack label <> ", but got " <> show other)
  where
    decodeItem = \case
      EPath path -> Right (resolveConfigPath root path)
      EString text -> Right (resolveConfigPath root (Text.unpack text))
      item -> Left ("expected path-like item in " <> Text.unpack label <> ", but got " <> show item)

expandDeclarationPackPath :: FilePath -> FilePath -> IO (Either String [DeclarationSupportFile])
expandDeclarationPackPath root path = do
  let normalized = normalise path
  isDir <- doesDirectoryExist normalized
  fileExists <- doesFileExist normalized
  if isDir
    then do
      files <- findDeclarationFiles normalized
      pure (Right (map (mkDeclarationSupportFile root) files))
    else
      if fileExists
        then
          if ".d.tnix" `isSuffixOf` normalized
            then pure (Right [mkDeclarationSupportFile root normalized])
            else pure (Left ("declarationPacks entries must point to .d.tnix files or directories, but got " <> normalized))
        else pure (Left ("declarationPacks entry does not exist: " <> normalized))

mkDeclarationSupportFile :: FilePath -> FilePath -> DeclarationSupportFile
mkDeclarationSupportFile root loadPath =
  DeclarationSupportFile
    { declarationLoadPath = loadPath,
      declarationResolvePath = maybe (normalise loadPath) (normalise . (root </>) . joinPath) (workspacePackRelativeParts loadPath)
    }

workspacePackRelativeParts :: FilePath -> Maybe [FilePath]
workspacePackRelativeParts path =
  findWorkspacePackSuffix (splitDirectories (normalise path))
  where
    findWorkspacePackSuffix ("registry" : "workspace" : rest) = Just ("registry" : "workspace" : rest)
    findWorkspacePackSuffix (_ : rest) = findWorkspacePackSuffix rest
    findWorkspacePackSuffix [] = Nothing

resolveConfigPath :: FilePath -> FilePath -> FilePath
resolveConfigPath root target
  | isAbsolute target = normalise target
  | otherwise = normalise (root </> dropDotSlash target)

dropDotSlash :: FilePath -> FilePath
dropDotSlash path =
  case path of
    '.' : '/' : rest -> rest
    _ -> path

mergeLoadedWorlds :: [DeclarationSupportFile] -> [World] -> Either String World
mergeLoadedWorlds files loaded = do
  ambient <- mergeAmbientWorlds (zip (map declarationLoadPath files) (map worldAmbient loaded))
  pure World {worldAliases = concatMap worldAliases loaded, worldAmbient = ambient}

loadDeclarationFile :: DeclarationSupportFile -> IO (Either String World)
loadDeclarationFile file = do
  let path = declarationLoadPath file
  inputResult <- readTextFile path
  pure $ do
    input <- inputResult
    program <- firstError ("failed to load declaration file " <> path <> ": ") (parseText path input)
    case markedValue <$> programExpr program of
      Just _ -> Left ("declaration files must not contain executable expressions: " <> path)
      Nothing -> do
        _ <- validateProgramKinds (programAliases program) program
        _ <- validateProgramIndexedTypes program
        ambient <- collectAmbientWithBase path (declarationResolvePath file) program
        pure World {worldAliases = programAliases program, worldAmbient = ambient}

collectAmbient :: FilePath -> Program -> Either String (Map FilePath Scheme)
collectAmbient file = collectAmbientWithBase file file

collectAmbientWithBase :: FilePath -> FilePath -> Program -> Either String (Map FilePath Scheme)
collectAmbientWithBase file resolveBase program = do
  let duplicates = duplicateNames (map (resolvePath resolveBase . ambientPath) (programAmbient program))
  case duplicates of
    dup : _ -> Left ("duplicate ambient declarations for target " <> show dup <> " in " <> file)
    [] -> Map.fromList <$> traverse toPair (programAmbient program)
  where
    toPair decl = do
      scheme <- schemeFromEntries (ambientEntries decl)
      pure (resolvePath resolveBase (ambientPath decl), scheme)
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

findWorkspaceDeclarationFiles :: FilePath -> IO [FilePath]
findWorkspaceDeclarationFiles root = go root
  where
    go dir = do
      names <- sort <$> listDirectory dir
      fmap concat $
        forM names $ \name -> do
          let path = dir </> name
          isDir <- doesDirectoryExist path
          if isDir
            then do
              nestedWorkspace <- if normalise path == normalise root then pure False else hasWorkspaceMarker path
              if nestedWorkspace
                then pure []
                else go path
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
        doesFileExist (dir </> "tnix.config.tnix"),
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

readTextFile :: FilePath -> IO (Either String Text)
readTextFile path = do
  result <- try @IOException (Text.readFile path)
  pure $
    case result of
      Left err -> Left ("failed to read " <> path <> ": " <> show err)
      Right input -> Right input

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
