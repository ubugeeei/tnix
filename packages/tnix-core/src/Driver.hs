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
import Control.Monad (forM)
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), isAbsolute, normalise, takeDirectory)
import Alias
import Check
import Compile
import Emit
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
    analysisBindings :: Map Name Scheme
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
    program <- parseText path input
    let aliases = mkAliasEnv (programAliases program <> worldAliases support)
        ambient = ambientFrom path program <> worldAmbient support
        context = CheckContext {checkAliases = aliases, checkAmbient = ambient, checkFile = path}
    result <- checkProgram context program
    pure Analysis {analysisProgram = program, analysisRoot = resultRoot result, analysisBindings = resultBindings result}

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

loadSupport :: FilePath -> IO World
loadSupport path = do
  files <- filter (/= normalise path) <$> findDeclarationFiles (takeDirectory path)
  worlds <- fmap rights (forM files loadDeclarationFile)
  pure World {worldAliases = concatMap worldAliases worlds, worldAmbient = Map.unions (map worldAmbient worlds)}
  where
    rights = foldr (\item acc -> either (const acc) (: acc) item) []

loadDeclarationFile :: FilePath -> IO (Either String World)
loadDeclarationFile path = do
  input <- Text.readFile path
  pure $ do
    program <- parseText path input
    pure World {worldAliases = programAliases program, worldAmbient = ambientFrom path program}

ambientFrom :: FilePath -> Program -> Map FilePath Scheme
ambientFrom file program = Map.fromList (map toPair (programAmbient program))
  where
    toPair decl = (resolvePath file (ambientPath decl), schemeFromEntries (ambientEntries decl))
    schemeFromEntries entries =
      case entries of
        [AmbientEntry "default" ty] -> schemeFromAnnotation ty
        _ -> Scheme [] (TRecord (Map.fromList [(ambientEntryName entry, ambientEntryType entry) | entry <- entries]))

findDeclarationFiles :: FilePath -> IO [FilePath]
findDeclarationFiles dir = do
  names <- listDirectory dir
  fmap concat $
    forM names $ \name -> do
      let path = dir </> name
      isDir <- doesDirectoryExist path
      if isDir
        then findDeclarationFiles path
        else pure [normalise path | ".d.tnix" `isSuffixOf` name]

resolvePath :: FilePath -> FilePath -> FilePath
resolvePath from target
  | isAbsolute target = normalise target
  | otherwise = normalise (takeDirectory from </> target)
