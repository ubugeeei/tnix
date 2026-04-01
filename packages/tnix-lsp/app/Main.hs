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
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Exit (exitSuccess)
import System.IO (stdin, stdout)
import Driver (Analysis (..), analyzeText)
import Server (applyContentChanges, asInt, asText, clearDiagnostics, clientCapabilities, documentPath, field, hoverResult, notify, publishDiagnostics, readMessage, respond, uriPath)

-- | Start the stdio event loop and keep the latest document text in memory.
main :: IO ()
main = do
  ref <- newIORef Map.empty
  forever $ readMessage stdin >>= maybe (pure ()) (handle ref)

-- | Dispatch one incoming JSON-RPC message.
handle :: IORef (Map.Map FilePath Text) -> Value -> IO ()
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
update :: IORef (Map.Map FilePath Text) -> Value -> IO (FilePath, Either String Analysis)
update ref msg = do
  docs <- readIORef ref
  let params = field "params" msg
      textDocument = params >>= field "textDocument"
      file = maybe "" uriPath (textDocument >>= field "uri" >>= asText)
      fullText = textDocument >>= field "text" >>= asText
      current = Map.lookup file docs
  contentResult <-
    case fullText of
      Just content -> pure (Right content)
      Nothing -> do
        base <- maybe (readFileSafe file) (pure . Right) current
        pure (base >>= (`applyContentChanges` params))
  case contentResult of
    Left err -> pure (file, Left err)
    Right content -> do
      modifyIORef' ref (Map.insert file content)
      result <- analyzeText file content
      pure (file, result)

-- | Publish diagnostics for the latest analysis result.
publish :: (FilePath, Either String Analysis) -> IO ()
publish (file, result) =
  notify stdout "textDocument/publishDiagnostics" (publishDiagnostics file result)

-- | Drop a document from the in-memory cache and clear its diagnostics.
closeDocument :: IORef (Map.Map FilePath Text) -> Value -> IO ()
closeDocument ref msg =
  case documentPath (field "params" msg) of
    Just file -> do
      modifyIORef' ref (Map.delete file)
      notify stdout "textDocument/publishDiagnostics" (clearDiagnostics file)
    Nothing -> pure ()

-- | Compute hover contents at the requested position.
hover :: IORef (Map.Map FilePath Text) -> Value -> IO Value
hover ref msg = do
  let params = field "params" msg
      textDocument = params >>= field "textDocument"
      position = params >>= field "position"
      file = maybe "" uriPath (textDocument >>= field "uri" >>= asText)
      lineNo = maybe 0 asInt (position >>= field "line")
      charNo = maybe 0 asInt (position >>= field "character")
  docs <- readIORef ref
  contentResult <- maybe (readFileSafe file) (pure . Right) (Map.lookup file docs)
  case contentResult of
    Left err -> pure (hoverResult (Left err) "" lineNo charNo)
    Right content -> do
      result <- analyzeText file content
      pure (hoverResult result content lineNo charNo)

readFileSafe :: FilePath -> IO (Either String Text)
readFileSafe file = do
  result <- try @IOException (TIO.readFile file)
  pure $
    case result of
      Left err -> Left ("failed to read " <> file <> ": " <> show err)
      Right content -> Right content
