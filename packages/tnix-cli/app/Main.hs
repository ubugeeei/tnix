{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Command-line entry point for tnix tooling.
--
-- The CLI intentionally stays small: it exposes the three capabilities that are
-- useful both for humans and for editor integration tests, namely compile,
-- check, and declaration emission.
module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Options.Applicative
import System.Exit (die)
import TnixDriver
import TnixPretty (renderScheme)

-- | Supported subcommands.
data Command
  = Compile FilePath (Maybe FilePath)
  | Check FilePath
  | Emit FilePath (Maybe FilePath)

-- | Parse arguments and execute the requested command.
main :: IO ()
main = execParser opts >>= run
  where
    opts = info (commandParser <**> helper) (fullDesc <> progDesc "Compile, check, and emit tnix files")

-- | Execute one CLI command.
run :: Command -> IO ()
run = \case
  Compile input output -> compileFile input >>= either die (writeOutput output)
  Check input -> analyzeFile input >>= either die report
  Emit input output -> emitFile input >>= either die (writeOutput output)

-- | Pretty-print the inferred root and bindings for `tnix check`.
report :: Analysis -> IO ()
report analysis = do
  maybe (pure ()) (\root -> Text.putStrLn ("root: " <> renderScheme root)) (analysisRoot analysis)
  mapM_ (\(name, scheme) -> Text.putStrLn (name <> " :: " <> renderScheme scheme)) (Map.toList (analysisBindings analysis))

-- | Write command output either to stdout or an explicit file.
writeOutput :: Maybe FilePath -> Text -> IO ()
writeOutput output content =
  maybe (Text.putStrLn content) (`Text.writeFile` content) output

-- | Command-line parser definition.
commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "compile" (info compileP (progDesc "Compile .tnix to .nix"))
        <> command "check" (info checkP (progDesc "Type-check a .tnix file"))
        <> command "emit" (info emitP (progDesc "Emit a .d.tnix declaration file"))
    )
  where
    fileArg = strArgument (metavar "FILE")
    outputOpt = optional (strOption (short 'o' <> long "output" <> metavar "OUTPUT"))
    compileP = Compile <$> fileArg <*> outputOpt
    checkP = Check <$> fileArg
    emitP = Emit <$> fileArg <*> outputOpt
