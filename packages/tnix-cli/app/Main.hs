{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Options.Applicative
import System.Exit (die)
import Tnix.Driver
import Tnix.Pretty (renderScheme)

data Command
  = Compile FilePath (Maybe FilePath)
  | Check FilePath
  | Emit FilePath (Maybe FilePath)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (commandParser <**> helper) (fullDesc <> progDesc "Compile, check, and emit tnix files")

run :: Command -> IO ()
run = \case
  Compile input output -> compileFile input >>= either die (writeOutput output)
  Check input -> analyzeFile input >>= either die report
  Emit input output -> emitFile input >>= either die (writeOutput output)

report :: Analysis -> IO ()
report analysis = do
  maybe (pure ()) (\root -> Text.putStrLn ("root: " <> renderScheme root)) (analysisRoot analysis)
  mapM_ (\(name, scheme) -> Text.putStrLn (name <> " :: " <> renderScheme scheme)) (Map.toList (analysisBindings analysis))

writeOutput :: Maybe FilePath -> Text -> IO ()
writeOutput output content =
  maybe (Text.putStrLn content) (`Text.writeFile` content) output

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
