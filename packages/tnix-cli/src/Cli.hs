{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Testable CLI helpers for tnix.
module Cli
  ( Command (..),
    commandParser,
    renderAnalysis,
    writeOutput,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Driver (Analysis (..))
import Options.Applicative
import Pretty (renderScheme)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | Supported subcommands.
data Command
  = Compile FilePath (Maybe FilePath)
  | Check FilePath
  | Emit FilePath (Maybe FilePath)
  deriving (Eq, Show)

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
