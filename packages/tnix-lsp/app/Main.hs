{-# LANGUAGE OverloadedStrings #-}

-- | JSON-RPC/LSP bridge for tnix.
--
-- The server keeps protocol framing and stdio orchestration here while pushing
-- semantic behavior into the core driver and the testable 'Session' helpers.
-- That makes hover, diagnostics, completion, and jump-to-definition available
-- to real editors without burying the logic inside an opaque event loop.
module Main (main) where

import Control.Monad (forever)
import Control.Exception (IOException, try)
import Data.Aeson
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import Paths_tnix_lsp qualified as PackageInfo
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Driver (Analysis (..), analyzeText)
import Session qualified
import Server (asText, clearDiagnostics, clientCapabilities, field, notify, publishDiagnostics, publishDiagnosticsWithContent, readMessage, respond)

-- | Start the stdio event loop and keep the latest document text in memory.
main :: IO ()
main = do
  args <- getArgs
  handleArgs args

handleArgs :: [String] -> IO ()
handleArgs args
  | any (`elem` ["--help", "-h"]) args = putStrLn helpText
  | any (`elem` ["--version", "-v"]) args = putStrLn versionText
  | null filteredArgs = runServer
  | otherwise = do
      hPutStrLn stderr ("tnix-lsp: unsupported arguments: " <> unwords filteredArgs)
      hPutStrLn stderr "Use --stdio, --version, or --help."
      exitFailure
  where
    filteredArgs = filter (/= "--stdio") args

runServer :: IO ()
runServer = do
  ref <- newIORef mempty
  forever $ readMessage stdin >>= maybe (pure ()) (handle ref)

helpText :: String
helpText =
  unlines
    [ "tnix-lsp",
      "",
      "Usage:",
      "  tnix-lsp [--stdio]",
      "  tnix-lsp --version",
      "  tnix-lsp --help"
    ]

versionText :: String
versionText = "tnix-lsp " <> showVersion PackageInfo.version

-- | Dispatch one incoming JSON-RPC message.
handle :: IORef Session.Documents -> Value -> IO ()
handle ref msg = case field "method" msg >>= asText of
  Just "initialize" -> respond stdout msg clientCapabilities
  Just "shutdown" -> respond stdout msg Null
  Just "exit" -> exitSuccess
  Just "textDocument/didOpen" -> update ref msg >>= publish
  Just "textDocument/didChange" -> update ref msg >>= publish
  Just "textDocument/didClose" -> closeDocument ref msg
  Just "textDocument/hover" -> hover ref msg >>= respond stdout msg
  Just "textDocument/completion" -> completion ref msg >>= respond stdout msg
  Just "textDocument/definition" -> definition ref msg >>= respond stdout msg
  Just "textDocument/declaration" -> definition ref msg >>= respond stdout msg
  Just "textDocument/references" -> references ref msg >>= respond stdout msg
  Just "textDocument/rename" -> rename ref msg >>= respond stdout msg
  Just "textDocument/documentSymbol" -> documentSymbols ref msg >>= respond stdout msg
  Just "workspace/symbol" -> workspaceSymbols ref msg >>= respond stdout msg
  Just "textDocument/codeAction" -> codeActions ref msg >>= respond stdout msg
  Just "textDocument/semanticTokens/full" -> semanticTokens ref msg >>= respond stdout msg
  _ -> pure ()

-- | Update the in-memory copy of a document and re-run analysis.
update :: IORef Session.Documents -> Value -> IO (FilePath, Maybe Text, Either String Analysis)
update ref msg = do
  docs <- readIORef ref
  (docs', file, result) <- Session.updateDocuments readFileSafe analyzeText docs msg
  writeIORef ref docs'
  pure (file, Map.lookup file docs', result)

-- | Publish diagnostics for the latest analysis result.
publish :: (FilePath, Maybe Text, Either String Analysis) -> IO ()
publish (file, content, result) =
  notify
    stdout
    "textDocument/publishDiagnostics"
    (maybe (publishDiagnostics file result) (\text -> publishDiagnosticsWithContent file text result) content)

-- | Drop a document from the in-memory cache and clear its diagnostics.
closeDocument :: IORef Session.Documents -> Value -> IO ()
closeDocument ref msg = do
  docs <- readIORef ref
  let (docs', closed) = Session.closeDocuments docs msg
  writeIORef ref docs'
  case closed of
    Just file -> notify stdout "textDocument/publishDiagnostics" (clearDiagnostics file)
    Nothing -> pure ()

-- | Compute hover contents at the requested position.
hover :: IORef Session.Documents -> Value -> IO Value
hover ref msg = do
  docs <- readIORef ref
  Session.hoverDocument readFileSafe analyzeText docs msg

-- | Compute completion results at the requested position.
completion :: IORef Session.Documents -> Value -> IO Value
completion ref msg = do
  docs <- readIORef ref
  Session.completionDocument readFileSafe analyzeText docs msg

-- | Resolve local or ambient definitions for the requested position.
definition :: IORef Session.Documents -> Value -> IO Value
definition ref msg = do
  docs <- readIORef ref
  Session.definitionDocument readFileSafe analyzeText docs msg

references :: IORef Session.Documents -> Value -> IO Value
references ref msg = do
  docs <- readIORef ref
  Session.referencesDocument readFileSafe analyzeText docs msg

rename :: IORef Session.Documents -> Value -> IO Value
rename ref msg = do
  docs <- readIORef ref
  Session.renameDocument readFileSafe analyzeText docs msg

documentSymbols :: IORef Session.Documents -> Value -> IO Value
documentSymbols ref msg = do
  docs <- readIORef ref
  Session.documentSymbolsDocument readFileSafe analyzeText docs msg

workspaceSymbols :: IORef Session.Documents -> Value -> IO Value
workspaceSymbols ref msg = do
  docs <- readIORef ref
  Session.workspaceSymbolsDocument readFileSafe analyzeText docs msg

codeActions :: IORef Session.Documents -> Value -> IO Value
codeActions ref msg = do
  docs <- readIORef ref
  Session.codeActionsDocument readFileSafe analyzeText docs msg

semanticTokens :: IORef Session.Documents -> Value -> IO Value
semanticTokens ref msg = do
  docs <- readIORef ref
  Session.semanticTokensDocument readFileSafe analyzeText docs msg

readFileSafe :: FilePath -> IO (Either String Text)
readFileSafe file = do
  result <- try @IOException (TIO.readFile file)
  pure $
    case result of
      Left err -> Left ("failed to read " <> file <> ": " <> show err)
      Right content -> Right content
