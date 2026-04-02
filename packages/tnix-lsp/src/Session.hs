{-# LANGUAGE OverloadedStrings #-}

-- | Testable document-session helpers for the tnix language server.
--
-- The executable keeps only the stdio loop and an 'IORef' cache. All document
-- lifecycle behavior lives here so specs can exercise the same update/hover
-- logic that the real server uses.
module Session
  ( Documents,
    closeDocuments,
    codeActionsDocument,
    completionDocument,
    definitionDocument,
    documentsFromList,
    documentSymbolsDocument,
    hoverDocument,
    lookupDocumentText,
    referencesDocument,
    renameDocument,
    semanticTokensDocument,
    updateDocuments,
    workspaceSymbolsDocument,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isAlphaNum, isDigit, isLetter, isUpper, toLower)
import Data.List (isPrefixOf, isSuffixOf, nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis (..))
import Server
  ( applyContentChanges,
    asInt,
    asText,
    completionResult,
    documentPath,
    field,
    findDefinitionRange,
    findFieldRange,
    hoverResult,
    location,
    pathUri,
    uriPath,
    wordAt,
  )
import Subtyping (resolveType)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), normalise, takeDirectory)
import Syntax
import Type

data CachedDocument = CachedDocument
  { cachedDocumentText :: Text,
    cachedDocumentAnalysis :: Maybe (Either String Analysis)
  }
  deriving (Eq, Show)

-- | In-memory document cache keyed by normalized file path.
newtype Documents = Documents {unDocuments :: Map FilePath CachedDocument}
  deriving (Eq, Show)

instance Semigroup Documents where
  Documents left <> Documents right = Documents (left <> right)

instance Monoid Documents where
  mempty = Documents Map.empty

documentsFromList :: [(FilePath, Text)] -> Documents
documentsFromList =
  Documents
    . Map.fromList
    . map (\(file, text) -> (normalise file, CachedDocument {cachedDocumentText = text, cachedDocumentAnalysis = Nothing}))

lookupDocumentText :: FilePath -> Documents -> Maybe Text
lookupDocumentText file (Documents docs) =
  cachedDocumentText <$> Map.lookup (normalise file) docs

data WorkspaceDocument = WorkspaceDocument
  { workspaceDocumentFile :: FilePath,
    workspaceDocumentContent :: Text,
    workspaceDocumentAnalysis :: Either String Analysis
  }

data IndexedSymbol = IndexedSymbol
  { indexedSymbolName :: Text,
    indexedSymbolKind :: Int,
    indexedSymbolFile :: FilePath,
    indexedSymbolRange :: (Int, Int, Int),
    indexedSymbolContainer :: Maybe Text
  }

data MatchMode
  = WholeWordMatch
  | FieldWordMatch

data ReferenceTarget = ReferenceTarget
  { referenceTargetFiles :: [FilePath],
    referenceTargetNeedle :: Text,
    referenceTargetMode :: MatchMode
  }

data SemanticToken = SemanticToken
  { semanticTokenLine :: Int,
    semanticTokenStart :: Int,
    semanticTokenLength :: Int,
    semanticTokenType :: Int
  }

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
      file = maybe "" (normalise . uriPath) (textDocument >>= field "uri" >>= asText)
      fullText = textDocument >>= field "text" >>= asText
      current = lookupDocumentText file docs
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
      pure (insertDocument file content (Just result) docs, file, result)

-- | Remove one document from the cache and return the cleared file path.
closeDocuments :: Documents -> Value -> (Documents, Maybe FilePath)
closeDocuments docs msg =
  case normalise <$> documentPath (field "params" msg) of
    Just file -> (deleteDocument file docs, Just file)
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
      file = maybe "" (normalise . uriPath) (textDocument >>= field "uri" >>= asText)
      lineNo = maybe 0 asInt (position >>= field "line")
      charNo = maybe 0 asInt (position >>= field "character")
  contentResult <- loadDocumentContent readDocument docs file
  case contentResult of
    Left err -> pure (hoverResult (Left err) "" lineNo charNo)
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
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
      result <- loadDocumentAnalysis readDocument analyze docs file
      pure (completionResult result content lineNo charNo)

-- | Resolve a definition/declaration jump for the requested position.
--
-- Top-level names resolve in the current buffer first and then fall back to a
-- workspace-wide symbol index. Dotted selections additionally search field
-- declarations so ambient APIs such as `builtins.map` and local attrset fields
-- behave like editor users expect.
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
      result <- loadDocumentAnalysis readDocument analyze docs file
      workspace <- loadWorkspaceDocuments readDocument analyze docs file
      builtinsFile <- findBuiltinsFile file
      pure $
        maybe Null
          (\(targetFile, targetLine, startChar, endChar) -> location targetFile targetLine startChar endChar)
          (resolveDefinitionLocation file content workspace builtinsFile result lineNo charNo)

-- | Compute references for the selected symbol.
--
-- Local names stay scoped to the active buffer, while dotted members search the
-- workspace so shared ambient surfaces and record-field APIs are discoverable.
referencesDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
referencesDocument readDocument analyze docs msg = do
  (file, lineNo, charNo, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left _ -> pure (toJSON ([] :: [Value]))
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      workspace <- loadWorkspaceDocuments readDocument analyze docs file
      builtinsFile <- findBuiltinsFile file
      pure . toJSON $
        case resolveReferenceTarget file content workspace builtinsFile result lineNo charNo of
          Nothing -> []
          Just target ->
            [ location path foundLine startChar endChar
              | doc <- workspaceDocumentsForTarget workspace target,
                let path = workspaceDocumentFile doc,
                (foundLine, startChar, endChar) <- symbolRanges (workspaceDocumentContent doc) (referenceTargetNeedle target) (referenceTargetMode target)
            ]

-- | Produce a workspace edit that renames the selected symbol.
--
-- The rename strategy mirrors 'referencesDocument': plain local names stay in
-- one file, while member-style names update dotted usages plus declaration
-- sites across the workspace.
renameDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
renameDocument readDocument analyze docs msg = do
  (file, lineNo, charNo, contentResult) <- requestDocument readDocument docs msg
  case (contentResult, field "params" msg >>= field "newName" >>= asText) of
    (Right content, Just newName)
      | not (Text.null newName) -> do
          result <- loadDocumentAnalysis readDocument analyze docs file
          workspace <- loadWorkspaceDocuments readDocument analyze docs file
          builtinsFile <- findBuiltinsFile file
          pure $
            case resolveReferenceTarget file content workspace builtinsFile result lineNo charNo of
              Nothing -> Null
              Just target ->
                let edits =
                      [ (path, map (\(foundLine, startChar, endChar) -> textEdit foundLine startChar endChar newName) ranges)
                        | doc <- workspaceDocumentsForTarget workspace target,
                          let path = workspaceDocumentFile doc,
                          let ranges = symbolRanges (workspaceDocumentContent doc) (referenceTargetNeedle target) (referenceTargetMode target),
                          not (null ranges)
                      ]
                 in workspaceEdit edits
    _ -> pure Null

-- | List document symbols in a flat, editor-friendly shape.
documentSymbolsDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
documentSymbolsDocument readDocument analyze docs msg = do
  (file, _, _, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left _ -> pure (toJSON ([] :: [Value]))
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      pure (toJSON (map indexedSymbolInformation (documentIndexedSymbols file content result)))

-- | Search symbols across the surrounding workspace.
workspaceSymbolsDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
workspaceSymbolsDocument readDocument analyze docs msg =
  case workspaceSeedFile docs of
    Nothing -> pure (toJSON ([] :: [Value]))
    Just seedFile -> do
      workspace <- loadWorkspaceDocuments readDocument analyze docs seedFile
      let query = Text.toCaseFold (fromMaybe "" (field "params" msg >>= field "query" >>= asText))
          matches symbol =
            Text.null query
              || query `Text.isInfixOf` Text.toCaseFold (indexedSymbolName symbol)
              || maybe False (query `Text.isInfixOf`) (Text.toCaseFold <$> indexedSymbolContainer symbol)
          symbols = take 200 (filter matches (workspaceIndexedSymbols workspace))
      pure (toJSON (map indexedSymbolInformation symbols))

-- | Offer quick fixes for current diagnostics.
--
-- The server surfaces lightweight escape hatches (`@tnix-ignore`,
-- `@tnix-expected`) and, for obvious misspellings, a rename replacement based
-- on nearby in-scope symbol names.
codeActionsDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
codeActionsDocument readDocument analyze docs msg = do
  (file, _, _, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left _ -> pure (toJSON ([] :: [Value]))
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      workspace <- loadWorkspaceDocuments readDocument analyze docs file
      let diagnostics = diagnosticPayloads msg
          candidates = nub (documentCandidateNames result <> map indexedSymbolName (workspaceIndexedSymbols workspace) <> ["builtins", "import"])
          actions =
            concatMap
              ( \diagnostic ->
                  let message = fromMaybe "" (field "message" diagnostic >>= asText)
                      fixes = directiveActions file content diagnostic
                      renameFix =
                        case (diagnosticRange diagnostic, diagnosticSymbolName message, closestCandidate candidates =<< diagnosticSymbolName message) of
                          (Just (lineNo, startChar, endChar), Just current, Just replacement)
                            | current /= replacement ->
                                [ quickFixAction
                                    ("Replace with `" <> replacement <> "`")
                                    file
                                    [textEdit lineNo startChar endChar replacement]
                                ]
                          _ -> []
                   in fixes <> renameFix
              )
              diagnostics
      pure (toJSON actions)

-- | Return LSP semantic tokens for one document.
semanticTokensDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
semanticTokensDocument readDocument analyze docs msg = do
  (file, _, _, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left _ -> pure (object ["data" .= ([] :: [Int])])
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      pure (object ["data" .= encodeSemanticTokens (semanticTokensFor content result)])

requestDocument ::
  (FilePath -> IO (Either String Text)) ->
  Documents ->
  Value ->
  IO (FilePath, Int, Int, Either String Text)
requestDocument readDocument docs msg = do
  let params = field "params" msg
      textDocument = params >>= field "textDocument"
      position = params >>= field "position"
      file = maybe "" (normalise . uriPath) (textDocument >>= field "uri" >>= asText)
      lineNo = maybe 0 asInt (position >>= field "line")
      charNo = maybe 0 asInt (position >>= field "character")
  contentResult <- loadDocumentContent readDocument docs file
  pure (file, lineNo, charNo, contentResult)

loadDocumentContent :: (FilePath -> IO (Either String Text)) -> Documents -> FilePath -> IO (Either String Text)
loadDocumentContent readDocument docs file =
  maybe (readDocument file) (pure . Right) (lookupDocumentText file docs)

loadDocumentAnalysis ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  FilePath ->
  IO (Either String Analysis)
loadDocumentAnalysis readDocument analyze docs file =
  case lookupCachedDocument file docs of
    Just cached ->
      case cachedDocumentAnalysis cached of
        Just result -> pure result
        Nothing -> analyze file (cachedDocumentText cached)
    Nothing -> do
      contentResult <- readDocument file
      case contentResult of
        Left err -> pure (Left err)
        Right content -> analyze file content

loadWorkspaceDocuments ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  FilePath ->
  IO [WorkspaceDocument]
loadWorkspaceDocuments readDocument analyze docs currentFile = do
  files <- workspaceFilesFor currentFile
  builtinsFile <- findBuiltinsFile currentFile
  let targets = nub (files <> maybe [] pure builtinsFile)
  forM targets $ \path -> do
    case lookupCachedDocument path docs of
      Just cached -> do
        result <-
          case cachedDocumentAnalysis cached of
            Just analysisResult -> pure analysisResult
            Nothing -> analyze path (cachedDocumentText cached)
        pure
          WorkspaceDocument
            { workspaceDocumentFile = path,
              workspaceDocumentContent = cachedDocumentText cached,
              workspaceDocumentAnalysis = result
            }
      Nothing -> do
        contentResult <- readDocument path
        case contentResult of
          Left err ->
            pure
              WorkspaceDocument
                { workspaceDocumentFile = path,
                  workspaceDocumentContent = "",
                  workspaceDocumentAnalysis = Left err
                }
          Right content -> do
            result <- analyze path content
            pure
              WorkspaceDocument
                { workspaceDocumentFile = path,
                  workspaceDocumentContent = content,
                  workspaceDocumentAnalysis = result
                }

workspaceDocumentsForTarget :: [WorkspaceDocument] -> ReferenceTarget -> [WorkspaceDocument]
workspaceDocumentsForTarget workspace target =
  filter (\doc -> workspaceDocumentFile doc `elem` referenceTargetFiles target) workspace

workspaceIndexedSymbols :: [WorkspaceDocument] -> [IndexedSymbol]
workspaceIndexedSymbols workspace =
  sortOn (\symbol -> (indexedSymbolName symbol, indexedSymbolFile symbol, indexedSymbolRange symbol)) $
    concatMap (\doc -> documentIndexedSymbols (workspaceDocumentFile doc) (workspaceDocumentContent doc) (workspaceDocumentAnalysis doc)) workspace

resolveDefinitionLocation ::
  FilePath ->
  Text ->
  [WorkspaceDocument] ->
  Maybe FilePath ->
  Either String Analysis ->
  Int ->
  Int ->
  Maybe (FilePath, Int, Int, Int)
resolveDefinitionLocation file content workspace builtinsFile result lineNo charNo =
  let symbol = wordAt lineNo charNo content
      parts = filter (not . Text.null) (Text.splitOn "." symbol)
      workspaceSymbols = workspaceIndexedSymbols workspace
      currentDefinitions = documentIndexedSymbols file content result
      currentFieldTarget name = (\(targetLine, startChar, endChar) -> (file, targetLine, startChar, endChar)) <$> (findFieldRange content name <|> findDefinitionRange content name)
      uniqueWorkspaceTarget name =
        case filter (\entry -> indexedSymbolName entry == name) workspaceSymbols of
          [entry] ->
            let (targetLine, startChar, endChar) = indexedSymbolRange entry
             in Just (indexedSymbolFile entry, targetLine, startChar, endChar)
          entries ->
            case filter (\entry -> indexedSymbolFile entry == file) entries of
              current : _ ->
                let (targetLine, startChar, endChar) = indexedSymbolRange current
                 in Just (indexedSymbolFile current, targetLine, startChar, endChar)
              [] -> Nothing
   in case parts of
        ["builtins", member] ->
          builtinsFile >>= \targetFile ->
            let targetDoc = listToMaybe (filter (\doc -> workspaceDocumentFile doc == targetFile) workspace)
             in case targetDoc of
                  Just doc ->
                    (\(targetLine, startChar, endChar) -> (targetFile, targetLine, startChar, endChar))
                      <$> findDefinitionRange (workspaceDocumentContent doc) member
                  Nothing -> Nothing
        [name]
          | any (\entry -> indexedSymbolName entry == name) currentDefinitions ->
              (\(targetLine, startChar, endChar) -> (file, targetLine, startChar, endChar)) <$> findDefinitionRange content name
          | otherwise -> uniqueWorkspaceTarget name
        _
          | Just fieldName <- listToMaybe (reverse parts) ->
              currentFieldTarget fieldName <|> uniqueWorkspaceTarget fieldName
        _ -> Nothing

resolveReferenceTarget ::
  FilePath ->
  Text ->
  [WorkspaceDocument] ->
  Maybe FilePath ->
  Either String Analysis ->
  Int ->
  Int ->
  Maybe ReferenceTarget
resolveReferenceTarget file content workspace builtinsFile result lineNo charNo =
  let symbol = wordAt lineNo charNo content
      parts = filter (not . Text.null) (Text.splitOn "." symbol)
      currentSymbols = documentIndexedSymbols file content result
      workspaceSymbols = workspaceIndexedSymbols workspace
      currentFileOnly name =
        Just ReferenceTarget {referenceTargetFiles = [file], referenceTargetNeedle = name, referenceTargetMode = WholeWordMatch}
      workspaceField name =
        Just
          ReferenceTarget
            { referenceTargetFiles = map workspaceDocumentFile workspace,
              referenceTargetNeedle = name,
              referenceTargetMode = FieldWordMatch
            }
      uniqueWorkspace name =
        case filter (\entry -> indexedSymbolName entry == name) workspaceSymbols of
          [_] -> workspaceField name
          _ -> Nothing
   in case parts of
        ["builtins", member] ->
          case builtinsFile of
            Just _ -> workspaceField member
            Nothing -> Nothing
        [name]
          | any (\entry -> indexedSymbolName entry == name) currentSymbols -> currentFileOnly name
          | ".d.tnix" `isSuffixOf` file -> uniqueWorkspace name <|> currentFileOnly name
          | otherwise -> currentFileOnly name
        _
          | Just fieldName <- listToMaybe (reverse parts) -> workspaceField fieldName
        _ -> Nothing

documentIndexedSymbols :: FilePath -> Text -> Either String Analysis -> [IndexedSymbol]
documentIndexedSymbols file content result =
  case result of
    Left _ -> []
    Right analysis ->
      sortOn (\symbol -> indexedSymbolRange symbol) $
        aliasSymbols analysis
          <> ambientModuleSymbols analysis
          <> ambientEntrySymbols analysis
          <> bindingSymbols analysis
          <> rootExportSymbols analysis
  where
    aliasSymbols analysis =
      mapMaybe
        ( \alias -> do
            range <- findDefinitionRange content (typeAliasName alias)
            pure
              IndexedSymbol
                { indexedSymbolName = typeAliasName alias,
                  indexedSymbolKind = 23,
                  indexedSymbolFile = file,
                  indexedSymbolRange = range,
                  indexedSymbolContainer = Just "type"
                }
        )
        (programAliases (analysisProgram analysis))

    ambientModuleSymbols analysis =
      mapMaybe
        ( \decl -> do
            range <- findDeclareRange content (ambientPath decl)
            pure
              IndexedSymbol
                { indexedSymbolName = Text.pack (ambientPath decl),
                  indexedSymbolKind = 2,
                  indexedSymbolFile = file,
                  indexedSymbolRange = range,
                  indexedSymbolContainer = Nothing
                }
        )
        (programAmbient (analysisProgram analysis))

    ambientEntrySymbols analysis =
      concatMap
        ( \decl ->
            mapMaybe
              ( \entry -> do
                  range <- findDefinitionRange content (ambientEntryName entry)
                  pure
                    IndexedSymbol
                      { indexedSymbolName = ambientEntryName entry,
                        indexedSymbolKind = kindForType (ambientEntryType entry),
                        indexedSymbolFile = file,
                        indexedSymbolRange = range,
                        indexedSymbolContainer = Just (Text.pack (ambientPath decl))
                      }
              )
              (ambientEntries decl)
        )
        (programAmbient (analysisProgram analysis))

    bindingSymbols analysis =
      mapMaybe
        ( \(name, scheme) -> do
            range <- findDefinitionRange content name
            pure
              IndexedSymbol
                { indexedSymbolName = name,
                  indexedSymbolKind = kindForType (schemeType scheme),
                  indexedSymbolFile = file,
                  indexedSymbolRange = range,
                  indexedSymbolContainer = Just "let"
                }
        )
        (Map.toList (analysisBindings analysis))

    rootExportSymbols analysis =
      case analysisRoot analysis of
        Nothing -> []
        Just scheme ->
          case resolveType (analysisAliases analysis) (schemeType scheme) of
            TRecord fields ->
              mapMaybe
                ( \(name, fieldTy) -> do
                    range <- findFieldRange content name <|> findDefinitionRange content name
                    pure
                      IndexedSymbol
                        { indexedSymbolName = name,
                          indexedSymbolKind = kindForType fieldTy,
                          indexedSymbolFile = file,
                          indexedSymbolRange = range,
                          indexedSymbolContainer = Just "default"
                        }
                )
                (Map.toList fields)
            _ -> []

indexedSymbolInformation :: IndexedSymbol -> Value
indexedSymbolInformation symbol =
  object $
    [ "name" .= indexedSymbolName symbol,
      "kind" .= indexedSymbolKind symbol,
      "location" .= locationFromRange (indexedSymbolFile symbol) (indexedSymbolRange symbol)
    ]
      <> maybe [] (\container -> ["containerName" .= container]) (indexedSymbolContainer symbol)

documentCandidateNames :: Either String Analysis -> [Text]
documentCandidateNames result =
  case result of
    Left _ -> []
    Right analysis ->
      nub $
        Map.keys (analysisBindings analysis)
          <> map typeAliasName (programAliases (analysisProgram analysis))
          <> concatMap (map ambientEntryName . ambientEntries) (programAmbient (analysisProgram analysis))

workspaceSeedFile :: Documents -> Maybe FilePath
workspaceSeedFile (Documents docs) = fst <$> Map.lookupMin docs

workspaceFilesFor :: FilePath -> IO [FilePath]
workspaceFilesFor file
  | null file = pure []
  | otherwise = do
      root <- findWorkspaceRoot file
      marked <- hasWorkspaceMarker root
      if marked
        then sortOn id . nub <$> go root
        else pure [normalise file]
  where
    go dir = do
      names <- sortOn id <$> listDirectory dir
      fmap concat . forM names $ \name -> do
        let path = normalise (dir </> name)
        isDir <- doesDirectoryExist path
        if isDir
          then
            if ignoredDirectory name
              then pure []
              else go path
          else
            if isSourceFile name
              then pure [path]
              else pure []

findWorkspaceRoot :: FilePath -> IO FilePath
findWorkspaceRoot path = go start
  where
    start = normalise (takeDirectory path)
    go dir = do
      marked <- hasWorkspaceMarker dir
      let parent = normalise (takeDirectory dir)
      if marked
        then pure dir
        else
          if parent == dir
            then pure start
            else go parent

hasWorkspaceMarker :: FilePath -> IO Bool
hasWorkspaceMarker dir =
  or
    <$> sequence
      [ doesFileExist (dir </> "flake.nix"),
        doesFileExist (dir </> "cabal.project"),
        doesFileExist (dir </> "pnpm-workspace.yaml"),
        doesFileExist (dir </> "tnix.config.tnix"),
        doesDirectoryExist (dir </> ".git")
      ]

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

ignoredDirectory :: FilePath -> Bool
ignoredDirectory name =
  name `elem` [".git", ".direnv", ".devenv", "node_modules", "dist"]
    || "result" `isPrefixOf` name

isSourceFile :: FilePath -> Bool
isSourceFile name = ".tnix" `isSuffixOf` name

symbolRanges :: Text -> Text -> MatchMode -> [(Int, Int, Int)]
symbolRanges content symbol mode =
  nub . sortOn id $
    case mode of
      WholeWordMatch -> allWordRanges content symbol
      FieldWordMatch -> allDefinitionRanges content symbol <> allFieldRanges content symbol

allDefinitionRanges :: Text -> Text -> [(Int, Int, Int)]
allDefinitionRanges content symbol =
  concatMap
    (\(lineNo, line) -> map (\(startChar, endChar) -> (lineNo, startChar, endChar)) (definitionSpans line symbol))
    (zip [0 ..] (Text.lines content))

allFieldRanges :: Text -> Text -> [(Int, Int, Int)]
allFieldRanges content symbol =
  concatMap
    (\(lineNo, line) -> map (\startChar -> (lineNo, startChar, startChar + Text.length symbol)) (fieldSpans line symbol))
    (zip [0 ..] (Text.lines content))

allWordRanges :: Text -> Text -> [(Int, Int, Int)]
allWordRanges content symbol =
  concatMap
    (\(lineNo, line) -> map (\startChar -> (lineNo, startChar, startChar + Text.length symbol)) (wordSpans line symbol))
    (zip [0 ..] (Text.lines content))

definitionSpans :: Text -> Text -> [(Int, Int)]
definitionSpans line symbol =
  let stripped = Text.stripStart line
      indent = Text.length line - Text.length stripped
      candidates =
        [ "type " <> symbol,
          symbol <> "::",
          symbol <> " ::",
          symbol <> "=",
          symbol <> " ="
        ]
   in nub
        [ (indent + startChar, indent + startChar + Text.length symbol)
          | candidate <- candidates,
            let (prefix, suffix) = Text.breakOn candidate stripped,
            not (Text.null suffix),
            let startChar = Text.length prefix + if "type " `Text.isPrefixOf` candidate then 5 else 0
        ]

fieldSpans :: Text -> Text -> [Int]
fieldSpans line symbol =
  [ Text.length prefix + 1
    | (prefix, _) <- Text.breakOnAll ("." <> symbol) line
  ]

wordSpans :: Text -> Text -> [Int]
wordSpans line symbol =
  mapMaybe validOffset offsets
  where
    offsets = map (Text.length . fst) (Text.breakOnAll symbol line)
    validOffset startChar =
      if boundary (startChar - 1) && boundary (startChar + Text.length symbol)
        then Just startChar
        else Nothing
    boundary ix
      | ix < 0 = True
      | ix >= Text.length line = True
      | otherwise = not (wordChar (Text.index line ix))

wordChar :: Char -> Bool
wordChar char =
  char == '_'
    || char == '-'
    || char == '\''
    || char == '?'
    || char == '!'
    || isAlphaNum char

findDeclareRange :: Text -> FilePath -> Maybe (Int, Int, Int)
findDeclareRange content path =
  listToMaybe $
    mapMaybe
      ( \(lineNo, line) ->
          let needles =
                [ "declare \"" <> Text.pack path <> "\"",
                  "declare " <> Text.pack path
                ]
           in listToMaybe
                [ (lineNo, startChar + 8, startChar + 8 + Text.length (Text.pack path))
                  | needle <- needles,
                    (prefix, suffix) <- Text.breakOnAll needle line,
                    not (Text.null suffix),
                    let startChar = Text.length prefix
                ]
      )
      (zip [0 ..] (Text.lines content))

kindForType :: Type -> Int
kindForType ty =
  case ty of
    TFun {} -> 12
    TRecord {} -> 19
    TLit (LString _) -> 15
    TLit (LInt _) -> 16
    TLit (LFloat _) -> 16
    TLit (LBool _) -> 17
    _ -> 13

textEdit :: Int -> Int -> Int -> Text -> Value
textEdit lineNo startChar endChar newText =
  object
    [ "range" .= rangeValue lineNo startChar endChar,
      "newText" .= newText
    ]

rangeValue :: Int -> Int -> Int -> Value
rangeValue lineNo startChar endChar =
  object
    [ "start" .= object ["line" .= lineNo, "character" .= startChar],
      "end" .= object ["line" .= lineNo, "character" .= endChar]
    ]

workspaceEdit :: [(FilePath, [Value])] -> Value
workspaceEdit edits =
  object
    [ "changes"
        .= Object
          ( KeyMap.fromList
              [ (Key.fromText (pathUri file), toJSON fileEdits)
                | (file, fileEdits) <- edits
              ]
          )
    ]

diagnosticPayloads :: Value -> [Value]
diagnosticPayloads msg =
  case field "params" msg >>= field "context" >>= field "diagnostics" of
    Just (Array diagnostics) -> toList diagnostics
    _ -> []
  where
    toList = foldr (:) []

diagnosticRange :: Value -> Maybe (Int, Int, Int)
diagnosticRange diagnostic = do
  range <- field "range" diagnostic
  start <- field "start" range
  ending <- field "end" range
  lineNo <- field "line" start
  startChar <- field "character" start
  endChar <- field "character" ending
  pure (asInt lineNo, asInt startChar, asInt endChar)

diagnosticSymbolName :: Text -> Maybe Text
diagnosticSymbolName message = do
  (_, suffix) <- listToMaybe (Text.breakOnAll "\"" message)
  let rest = Text.drop 1 suffix
      (quoted, trailing) = Text.breakOn "\"" rest
  if Text.null trailing then Nothing else Just quoted

directiveActions :: FilePath -> Text -> Value -> [Value]
directiveActions file content diagnostic =
  case diagnosticRange diagnostic of
    Just (lineNo, _, _)
      | not (lineHasDirective "# @tnix-ignore" lineNo content) ->
          [ quickFixAction "Add `# @tnix-ignore`" file [insertLineEdit lineNo "# @tnix-ignore\n"],
            quickFixAction "Add `# @tnix-expected`" file [insertLineEdit lineNo "# @tnix-expected\n"]
          ]
      | otherwise -> []
    Nothing -> []

insertLineEdit :: Int -> Text -> Value
insertLineEdit lineNo newText =
  object
    [ "range"
        .= object
          [ "start" .= object ["line" .= lineNo, "character" .= (0 :: Int)],
            "end" .= object ["line" .= lineNo, "character" .= (0 :: Int)]
          ],
      "newText" .= newText
    ]

lineHasDirective :: Text -> Int -> Text -> Bool
lineHasDirective directive lineNo content =
  case if lineNo <= 0 then [] else drop (lineNo - 1) (Text.lines content) of
    previous : _ -> directive `Text.isPrefixOf` Text.stripStart previous
    [] -> False

quickFixAction :: Text -> FilePath -> [Value] -> Value
quickFixAction title file edits =
  object
    [ "title" .= title,
      "kind" .= ("quickfix" :: Text),
      "edit" .= workspaceEdit [(file, edits)]
    ]

closestCandidate :: [Text] -> Text -> Maybe Text
closestCandidate candidates needle =
  case sortOn (\candidate -> (nameDistance needle candidate, candidate)) (filter (/= needle) candidates) of
    candidate : _
      | nameDistance needle candidate <= max 2 (Text.length needle `div` 2) -> Just candidate
    _ -> Nothing

nameDistance :: Text -> Text -> Int
nameDistance left right = last (foldl' step [0 .. length rightChars] (zip [1 ..] leftChars))
  where
    leftChars = map toLower (Text.unpack left)
    rightChars = map toLower (Text.unpack right)
    step previousRow (rowIndex, leftChar) =
      scanl
        (\leftCost (columnIndex, rightChar) -> minimum [leftCost + 1, previousRow !! columnIndex + 1, previousRow !! (columnIndex - 1) + substitutionCost leftChar rightChar])
        rowIndex
        (zip [1 ..] rightChars)
    substitutionCost leftChar rightChar
      | leftChar == rightChar = 0
      | otherwise = 1

semanticTokensFor :: Text -> Either String Analysis -> [SemanticToken]
semanticTokensFor content result =
  let functionNames = case result of
        Left _ -> []
        Right analysis ->
          [ name
            | (name, scheme) <- Map.toList (analysisBindings analysis),
              case schemeType scheme of
                TFun {} -> True
                _ -> False
          ]
      typeNames = case result of
        Left _ -> []
        Right analysis -> map typeAliasName (programAliases (analysisProgram analysis))
      rootFieldNames = case result of
        Left _ -> []
        Right analysis ->
          case analysisRoot analysis of
            Just scheme ->
              case resolveType (analysisAliases analysis) (schemeType scheme) of
                TRecord fields -> Map.keys fields
                _ -> []
            Nothing -> []
   in concatMap (\(lineNo, line) -> semanticTokensForLine functionNames typeNames rootFieldNames lineNo line) (zip [0 ..] (Text.lines content))

semanticTokensForLine :: [Text] -> [Text] -> [Text] -> Int -> Text -> [SemanticToken]
semanticTokensForLine functionNames typeNames rootFieldNames lineNo line = go 0 []
  where
    go index acc
      | index >= Text.length line = reverse acc
      | "#" `Text.isPrefixOf` Text.drop index line = reverse acc
      | otherwise =
          case Text.drop index line of
            rest
              | Just token <- annotate (stringToken index rest) -> go (index + semanticTokenLength token) (token : acc)
              | Just token <- annotate (numberToken index rest) -> go (index + semanticTokenLength token) (token : acc)
              | Just token <- annotate (operatorToken index rest) -> go (index + semanticTokenLength token) (token : acc)
              | Just (tokenText, width) <- identifierToken rest ->
                  let token =
                        SemanticToken
                          { semanticTokenLine = lineNo,
                            semanticTokenStart = index,
                            semanticTokenLength = width,
                            semanticTokenType = classifyIdentifier functionNames typeNames rootFieldNames line index tokenText
                          }
                   in go (index + width) (token : acc)
              | otherwise -> go (index + 1) acc
    annotate = fmap (\token -> token {semanticTokenLine = lineNo})

stringToken :: Int -> Text -> Maybe SemanticToken
stringToken index rest = do
  ('"', _) <- Text.uncons rest
  let body = Text.drop 1 rest
      len =
        case Text.findIndex (== '"') body of
          Just endIx -> endIx + 2
          Nothing -> Text.length rest
  pure SemanticToken {semanticTokenLine = 0, semanticTokenStart = index, semanticTokenLength = len, semanticTokenType = 5}

numberToken :: Int -> Text -> Maybe SemanticToken
numberToken index rest = do
  (char, _) <- Text.uncons rest
  if isDigit char
    then
      let width = Text.length (Text.takeWhile numberChar rest)
       in Just SemanticToken {semanticTokenLine = 0, semanticTokenStart = index, semanticTokenLength = width, semanticTokenType = 6}
    else Nothing
  where
    numberChar c = isDigit c || c `elem` (".eE+-" :: String)

operatorToken :: Int -> Text -> Maybe SemanticToken
operatorToken index rest =
  listToMaybe
    [ SemanticToken {semanticTokenLine = 0, semanticTokenStart = index, semanticTokenLength = Text.length operator, semanticTokenType = 7}
      | operator <- ["::", "->", "%1", ".", "=", "|", "?", ":"],
        operator `Text.isPrefixOf` rest
    ]

identifierToken :: Text -> Maybe (Text, Int)
identifierToken rest = do
  (char, _) <- Text.uncons rest
  if identStart char
    then
      let tokenText = Text.takeWhile identChar rest
       in Just (tokenText, Text.length tokenText)
    else Nothing

classifyIdentifier :: [Text] -> [Text] -> [Text] -> Text -> Int -> Text -> Int
classifyIdentifier functionNames typeNames rootFieldNames line index token
  | token `elem` reservedWords = 0
  | token `elem` typeNames = 1
  | token `elem` rootFieldNames = 4
  | previousNonSpace line index == Just '.' = 4
  | nextOperator line (index + Text.length token) == Just "::" =
      if lineStartsWithType line
        then 1
        else if token `elem` functionNames then 2 else 4
  | nextOperator line (index + Text.length token) == Just "=" =
      if token `elem` functionNames then 2 else 3
  | Text.any isUpper token && maybe False isUpper (fst <$> Text.uncons token) = 1
  | token `elem` functionNames = 2
  | otherwise = 3

previousNonSpace :: Text -> Int -> Maybe Char
previousNonSpace line index =
  listToMaybe
    [ char
      | char <- reverse (Text.unpack (Text.take index line)),
        not (char `elem` [' ', '\t'])
    ]

nextOperator :: Text -> Int -> Maybe Text
nextOperator line index =
  let suffix = Text.dropWhile (`elem` [' ', '\t']) (Text.drop index line)
   in listToMaybe
        [ operator
          | operator <- ["::", "=", ":"],
            operator `Text.isPrefixOf` suffix
        ]

lineStartsWithType :: Text -> Bool
lineStartsWithType = ("type " `Text.isPrefixOf`) . Text.stripStart

identStart :: Char -> Bool
identStart char = isLetter char || char == '_'

identChar :: Char -> Bool
identChar char = isAlphaNum char || char `elem` ("_'-" :: String)

reservedWords :: [Text]
reservedWords =
  ["any", "as", "declare", "dynamic", "else", "extends", "false", "forall", "if", "in", "infer", "inherit", "let", "null", "then", "true", "type", "unknown"]

encodeSemanticTokens :: [SemanticToken] -> [Int]
encodeSemanticTokens tokens = snd (foldl step (Nothing, []) (sortOn (\token -> (semanticTokenLine token, semanticTokenStart token)) tokens))
  where
    step (previous, acc) token =
      let deltaLine = maybe (semanticTokenLine token) (\prev -> semanticTokenLine token - semanticTokenLine prev) previous
          deltaStart =
            case previous of
              Just prev
                | semanticTokenLine prev == semanticTokenLine token ->
                    semanticTokenStart token - semanticTokenStart prev
              _ -> semanticTokenStart token
          encoded =
            [ deltaLine,
              deltaStart,
              semanticTokenLength token,
              semanticTokenType token,
              0
            ]
       in (Just token, acc <> encoded)

locationFromRange :: FilePath -> (Int, Int, Int) -> Value
locationFromRange file (lineNo, startChar, endChar) = location file lineNo startChar endChar

lookupCachedDocument :: FilePath -> Documents -> Maybe CachedDocument
lookupCachedDocument file (Documents docs) = Map.lookup (normalise file) docs

insertDocument :: FilePath -> Text -> Maybe (Either String Analysis) -> Documents -> Documents
insertDocument file content analysis (Documents docs) =
  Documents (Map.insert (normalise file) CachedDocument {cachedDocumentText = content, cachedDocumentAnalysis = analysis} docs)

deleteDocument :: FilePath -> Documents -> Documents
deleteDocument file (Documents docs) = Documents (Map.delete (normalise file) docs)
