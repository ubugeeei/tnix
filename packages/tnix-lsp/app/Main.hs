{-# LANGUAGE OverloadedStrings #-}

-- | Minimal JSON-RPC/LSP bridge for tnix.
--
-- This server intentionally implements only the core interactions needed to
-- prove the architecture: opening/changing documents, diagnostics, and hover.
-- It delegates all semantic work to 'Driver'.
module Main (main) where

import Control.Monad (forever)
import Data.Aeson
import Control.Exception (IOException, try)
import Data.IORef
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Exit (exitSuccess)
import System.IO (stdin, stdout)
import Driver (Analysis (..), analyzeText)
import Session qualified
import Server (asText, clearDiagnostics, clientCapabilities, field, notify, publishDiagnostics, readMessage, respond)

-- | Start the stdio event loop and keep the latest document text in memory.
main :: IO ()
main = do
  ref <- newIORef mempty
  forever $ readMessage stdin >>= maybe (pure ()) (handle ref)

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
  _ -> pure ()

-- | Update the in-memory copy of a document and re-run analysis.
update :: IORef Session.Documents -> Value -> IO (FilePath, Either String Analysis)
update ref msg = do
  docs <- readIORef ref
  (docs', file, result) <- Session.updateDocuments readFileSafe analyzeText docs msg
  writeIORef ref docs'
  pure (file, result)

-- | Publish diagnostics for the latest analysis result.
publish :: (FilePath, Either String Analysis) -> IO ()
publish (file, result) =
  notify stdout "textDocument/publishDiagnostics" (publishDiagnostics file result)

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

readFileSafe :: FilePath -> IO (Either String Text)
readFileSafe file = do
  result <- try @IOException (TIO.readFile file)
  pure $
    case result of
      Left err -> Left ("failed to read " <> file <> ": " <> show err)
      Right content -> Right content
