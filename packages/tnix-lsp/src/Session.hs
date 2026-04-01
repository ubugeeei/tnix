{-# LANGUAGE OverloadedStrings #-}

-- | Testable document-session helpers for the tnix language server.
--
-- The executable keeps only the stdio loop and an 'IORef' cache. All document
-- lifecycle behavior lives here so specs can exercise the same update/hover
-- logic that the real server uses.
module Session
  ( Documents,
    closeDocuments,
    completionDocument,
    definitionDocument,
    hoverDocument,
    updateDocuments,
  )
where

import Data.Aeson (Value (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis)
import Server (applyContentChanges, asInt, asText, completionResult, documentPath, field, hoverResult, location, uriPath, wordAt)
import System.Directory (doesFileExist)
import System.FilePath ((</>), normalise, takeDirectory)

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

-- | Compute completion items for the requested position.
--
-- Completion analyzes the latest cached text so editor suggestions can follow
-- unsaved changes. The payload is still useful when analysis fails because the
-- helper returns an empty, well-formed completion list instead of crashing the
-- protocol exchange.
completionDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
completionDocument readDocument analyze docs msg = do
  (file, lineNo, charNo, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left err -> pure (completionResult (Left err) "" lineNo charNo)
    Right content -> do
      result <- analyze file content
      pure (completionResult result content lineNo charNo)

-- | Resolve a definition/declaration jump for the requested position.
--
-- Local bindings and signatures resolve inside the current buffer. `builtins`
-- members additionally jump into the nearest `builtins.d.tnix` support file so
-- ambient declarations behave like a real standard library surface.
definitionDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
definitionDocument readDocument analyze docs msg = do
  (file, lineNo, charNo, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left _ -> pure Null
    Right content -> do
      result <- analyze file content
      builtinsFile <- findBuiltinsFile file
      case definitionTarget file builtinsFile (wordAt lineNo charNo content) of
        Nothing -> pure Null
        Just (targetFile, targetName) -> do
          targetContentResult <-
            if targetFile == file
              then pure (Right content)
              else readDocument targetFile
          pure $
            case (result, targetContentResult) of
              (Right _, Right targetContent) ->
                maybe Null (\(targetLine, startChar, endChar) -> location targetFile targetLine startChar endChar) (findDefinitionRange targetContent targetName)
              _ -> Null
  where
    definitionTarget currentFile builtinsFile symbol =
      case filter (not . Text.null) (Text.splitOn "." symbol) of
        [] -> fmap (\name -> (currentFile, name)) (nonEmpty symbol)
        ["builtins", name] -> fmap (\targetFile -> (targetFile, name)) builtinsFile
        name : _ -> Just (currentFile, name)
    nonEmpty text
      | Text.null text = Nothing
      | otherwise = Just text

requestDocument ::
  (FilePath -> IO (Either String Text)) ->
  Documents ->
  Value ->
  IO (FilePath, Int, Int, Either String Text)
requestDocument readDocument docs msg = do
  let params = field "params" msg
      textDocument = params >>= field "textDocument"
      position = params >>= field "position"
      file = maybe "" uriPath (textDocument >>= field "uri" >>= asText)
      lineNo = maybe 0 asInt (position >>= field "line")
      charNo = maybe 0 asInt (position >>= field "character")
  contentResult <- maybe (readDocument file) (pure . Right) (Map.lookup file docs)
  pure (file, lineNo, charNo, contentResult)

findBuiltinsFile :: FilePath -> IO (Maybe FilePath)
findBuiltinsFile file = go (normalise (takeDirectory file))
  where
    go dir = do
      let candidate = dir </> "builtins.d.tnix"
          parent = normalise (takeDirectory dir)
      exists <- doesFileExist candidate
      if exists
        then pure (Just candidate)
        else
          if parent == dir
            then pure Nothing
            else go parent

findDefinitionRange :: Text -> Text -> Maybe (Int, Int, Int)
findDefinitionRange content symbol =
  foldr firstMatch Nothing (zip [0 ..] (Text.lines content))
  where
    firstMatch (lineNo, line) acc =
      case definitionColumn line symbol of
        Just (startChar, endChar) -> Just (lineNo, startChar, endChar)
        Nothing -> acc

definitionColumn :: Text -> Text -> Maybe (Int, Int)
definitionColumn line symbol =
  let stripped = Text.stripStart line
      indent = Text.length line - Text.length stripped
      candidates =
        [ "type " <> symbol,
          symbol <> "::",
          symbol <> " ::",
          symbol <> "=",
          symbol <> " ="
        ]
   in foldr
        (\candidate acc ->
           case acc of
             Just _ -> acc
             Nothing ->
               let (prefix, suffix) = Text.breakOn candidate stripped
                   startOffset = if "type " `Text.isPrefixOf` candidate then 5 else 0
                in if Text.null suffix
                     then Nothing
                     else Just (indent + Text.length prefix + startOffset, indent + Text.length prefix + startOffset + Text.length symbol))
        Nothing
        candidates
