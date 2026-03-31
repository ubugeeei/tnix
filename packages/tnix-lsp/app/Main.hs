{-# LANGUAGE OverloadedStrings #-}

-- | Minimal JSON-RPC/LSP bridge for tnix.
--
-- This server intentionally implements only the core interactions needed to
-- prove the architecture: opening/changing documents, diagnostics, and hover.
-- It delegates all semantic work to 'Driver'.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (forever)
import Data.Aeson
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Exit (exitSuccess)
import System.IO (stdin, stdout)
import Driver (Analysis (..), analyzeText)
import Server (asInt, asText, field, firstChange, hoverResult, notify, publishDiagnostics, readMessage, respond, uriPath)

-- | Start the stdio event loop and keep the latest document text in memory.
main :: IO ()
main = do
  ref <- newIORef Map.empty
  forever $ readMessage stdin >>= maybe (pure ()) (handle ref)

-- | Dispatch one incoming JSON-RPC message.
handle :: IORef (Map.Map FilePath Text) -> Value -> IO ()
handle ref msg = case field "method" msg >>= asText of
  Just "initialize" -> respond stdout msg (object ["capabilities" .= object ["hoverProvider" .= True, "textDocumentSync" .= (1 :: Int)]])
  Just "shutdown" -> respond stdout msg Null
  Just "exit" -> exitSuccess
  Just "textDocument/didOpen" -> update ref msg >>= publish
  Just "textDocument/didChange" -> update ref msg >>= publish
  Just "textDocument/hover" -> hover ref msg >>= respond stdout msg
  _ -> pure ()

-- | Update the in-memory copy of a document and re-run analysis.
update :: IORef (Map.Map FilePath Text) -> Value -> IO (FilePath, Either String Analysis)
update ref msg = do
  let params = field "params" msg
      textDocument = params >>= field "textDocument"
      file = maybe "" uriPath (textDocument >>= field "uri" >>= asText)
      text' = ((textDocument <|> firstChange params) >>= field "text") >>= asText
  content <- maybe (TIO.readFile file) pure text'
  modifyIORef' ref (Map.insert file content)
  result <- analyzeText file content
  pure (file, result)

-- | Publish diagnostics for the latest analysis result.
publish :: (FilePath, Either String Analysis) -> IO ()
publish (file, result) =
  notify stdout "textDocument/publishDiagnostics" (publishDiagnostics file result)

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
  content <- maybe (TIO.readFile file) pure (Map.lookup file docs)
  result <- analyzeText file content
  pure (hoverResult result content lineNo charNo)
