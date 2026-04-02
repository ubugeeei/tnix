{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Command-line entry point for tnix tooling.
--
-- The CLI intentionally stays small: it exposes the three capabilities that are
-- useful both for humans and for editor integration tests, namely compile,
-- check, and declaration emission.
module Main (main) where

import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Cli qualified
import Options.Applicative
import Paths_tnix_cli qualified as PackageInfo
import System.Exit (die)

-- | Parse arguments and execute the requested command.
main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (Cli.commandParser <**> helper <**> versionOption)
        (fullDesc <> progDesc "Compile, check, emit, and scaffold tnix projects")
    versionOption =
      infoOption
        ("tnix " <> showVersion PackageInfo.version)
        (long "version" <> short 'v' <> help "Show the tnix version")

-- | Execute one CLI command.
run :: Cli.Command -> IO ()
run cmd =
  Cli.executeCommand cmd >>= \case
    Left err -> die err
    Right content ->
      case Cli.commandOutputPath cmd of
        Just output -> Cli.writeOutput (Just output) content
        Nothing -> Text.putStr content
