{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Testable CLI helpers for tnix.
module Cli
  ( Command (..),
    commandOutputPath,
    commandParser,
    executeCommand,
    renderAnalysis,
    writeOutput,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Driver (Analysis (..), analyzeFile, compileFile, emitFile)
import Options.Applicative
import Pretty (renderScheme)
import Project (initProject, scaffoldProject)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | Supported subcommands.
data Command
  = Compile FilePath (Maybe FilePath)
  | Check FilePath
  | Emit FilePath (Maybe FilePath)
  | Init (Maybe FilePath)
  | Scaffold (Maybe FilePath)
  deriving (Eq, Show)

-- | Command-line parser definition.
commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "compile" (info compileP (progDesc "Compile .tnix to .nix"))
        <> command "check" (info checkP (progDesc "Type-check a .tnix file"))
        <> command "emit" (info emitP (progDesc "Emit a .d.tnix declaration file"))
        <> command "init" (info initP (progDesc "Create tnix.config.tnix and starter files"))
        <> command "scaffold" (info scaffoldP (progDesc "Create project files from tnix.config.tnix"))
    )
  where
    fileArg = strArgument (metavar "FILE")
    dirArg = optional (strArgument (metavar "DIRECTORY"))
    outputOpt = optional (strOption (short 'o' <> long "output" <> metavar "OUTPUT"))
    compileP = Compile <$> fileArg <*> outputOpt
    checkP = Check <$> fileArg
    emitP = Emit <$> fileArg <*> outputOpt
    initP = Init <$> dirArg
    scaffoldP = Scaffold <$> dirArg

-- | Extract the explicit destination path carried by a command, if any.
--
-- `check` always prints to stdout, while `compile` and `emit` optionally write
-- to a file. Keeping this logic in one helper lets the executable stay tiny and
-- keeps tests focused on command semantics instead of argument plumbing.
commandOutputPath :: Command -> Maybe FilePath
commandOutputPath cmd =
  case cmd of
    Compile _ output -> output
    Check _ -> Nothing
    Emit _ output -> output
    Init _ -> Nothing
    Scaffold _ -> Nothing

-- | Execute one CLI command and return the rendered text payload.
--
-- The executable is responsible only for routing this text to stdout or a
-- file. Returning a plain 'Text' here gives the test suite a stable seam for
-- end-to-end command coverage without having to spawn a subprocess.
executeCommand :: Command -> IO (Either String Text)
executeCommand cmd =
  case cmd of
    Compile input _ -> compileFile input
    Check input -> fmap renderAnalysis <$> analyzeFile input
    Emit input _ -> emitFile input
    Init target -> initProject target
    Scaffold target -> scaffoldProject target

-- | Pretty-print the inferred root and bindings for `tnix check`.
renderAnalysis :: Analysis -> Text
renderAnalysis analysis =
  Text.unlines $
    maybe [] (\root -> ["root: " <> renderScheme root]) (analysisRoot analysis)
      <> [name <> " :: " <> renderScheme scheme | (name, scheme) <- Map.toList (analysisBindings analysis)]

-- | Write command output either to stdout or an explicit file.
writeOutput :: Maybe FilePath -> Text -> IO ()
writeOutput output content =
  case output of
    Nothing -> TextIO.putStrLn content
    Just path -> do
      createDirectoryIfMissing True (takeDirectory path)
      TextIO.writeFile path content
