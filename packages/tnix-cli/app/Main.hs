{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Command-line entry point for tnix tooling.
--
-- The CLI intentionally stays small: it exposes the three capabilities that are
-- useful both for humans and for editor integration tests, namely compile,
-- check, and declaration emission.
module Main (main) where

import Data.Text.IO qualified as Text
import Cli (Command (..), commandParser, renderAnalysis, writeOutput)
import Options.Applicative
import System.Exit (die)
import Driver (analyzeFile, compileFile, emitFile)

-- | Parse arguments and execute the requested command.
main :: IO ()
main = execParser opts >>= run
  where
    opts = info (commandParser <**> helper) (fullDesc <> progDesc "Compile, check, and emit tnix files")

-- | Execute one CLI command.
run :: Command -> IO ()
run = \case
  Compile input output -> compileFile input >>= either die (writeOutput output)
  Check input -> analyzeFile input >>= either die (Text.putStr . renderAnalysis)
  Emit input output -> emitFile input >>= either die (writeOutput output)
