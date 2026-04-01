{-# LANGUAGE OverloadedStrings #-}

-- | Testable document-session helpers for the tnix language server.
--
-- The executable keeps only the stdio loop and an 'IORef' cache. All document
-- lifecycle behavior lives here so specs can exercise the same update/hover
-- logic that the real server uses.
module Session
  ( Documents,
    closeDocuments,
    hoverDocument,
    updateDocuments,
  )
where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Driver (Analysis)
import Server (applyContentChanges, asInt, asText, documentPath, field, hoverResult, uriPath)

-- | In-memory document cache keyed by normalized file path.
type Documents = Map FilePath Text

-- | Update or open one document and re-run semantic analysis.
--
-- Full-text updates replace the cached content directly. Incremental changes
-- reuse the cached buffer when possible and fall back to reading from disk only
-- when the editor has not opened the file yet.
updateDocuments ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO (Documents, FilePath, Either String Analysis)
updateDocuments readDocument analyze docs msg = do
  let params = field "params" msg
      textDocument = params >>= field "textDocument"
      file = maybe "" uriPath (textDocument >>= field "uri" >>= asText)
      fullText = textDocument >>= field "text" >>= asText
      current = Map.lookup file docs
  contentResult <-
    case fullText of
      Just content -> pure (Right content)
      Nothing -> do
        base <- maybe (readDocument file) (pure . Right) current
        pure (base >>= (`applyContentChanges` params))
  case contentResult of
    Left err -> pure (docs, file, Left err)
    Right content -> do
      result <- analyze file content
      pure (Map.insert file content docs, file, result)

-- | Remove one document from the cache and return the cleared file path.
closeDocuments :: Documents -> Value -> (Documents, Maybe FilePath)
closeDocuments docs msg =
  case documentPath (field "params" msg) of
    Just file -> (Map.delete file docs, Just file)
    Nothing -> (docs, Nothing)

-- | Compute hover information for the requested position.
--
-- Hover prefers the cached document text so editors see immediate results after
-- unsaved edits. When the file is not cached yet, the helper falls back to
-- disk and renders a readable error when loading fails.
hoverDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
hoverDocument readDocument analyze docs msg = do
  let params = field "params" msg
      textDocument = params >>= field "textDocument"
      position = params >>= field "position"
      file = maybe "" uriPath (textDocument >>= field "uri" >>= asText)
      lineNo = maybe 0 asInt (position >>= field "line")
      charNo = maybe 0 asInt (position >>= field "character")
  contentResult <- maybe (readDocument file) (pure . Right) (Map.lookup file docs)
  case contentResult of
    Left err -> pure (hoverResult (Left err) "" lineNo charNo)
    Right content -> do
      result <- analyze file content
      pure (hoverResult result content lineNo charNo)
