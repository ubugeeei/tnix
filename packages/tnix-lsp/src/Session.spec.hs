{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Driver (Analysis (..), parseText)
import Session
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, openTempFile)
import Syntax
import Test.Hspec
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "updateDocuments" $ do
    it "stores full didOpen text and returns analysis" $ do
      let msg = openMessage "/tmp/main.tnix" "1"
      (docs, file, result) <- updateDocuments readNever analyzeStub mempty msg
      file `shouldBe` "/tmp/main.tnix"
      lookupDocumentText file docs `shouldBe` Just "1"
      fmap analysisRoot result `shouldBe` Right (Just (Scheme [] tInt))

    it "applies incremental changes against cached documents" $ do
      let docs = documentsFromList [("/tmp/main.tnix", "let value = 1;\n")]
          msg = changeMessage "/tmp/main.tnix" [replaceRange 0 4 0 9 "result", replaceRange 0 13 0 14 "2"]
      (docs', _, result) <- updateDocuments readNever analyzeStub docs msg
      lookupDocumentText "/tmp/main.tnix" docs' `shouldBe` Just "let result = 2;\n"
      fmap analysisBindings result `shouldBe` Right (Map.fromList [("result", Scheme [] tInt)])

    it "falls back to disk for uncached incremental updates" $ do
      let msg = changeMessage "/tmp/main.tnix" [replaceRange 0 0 0 1 "2"]
      (docs, file, result) <- updateDocuments (\_ -> pure (Right "1")) analyzeStub mempty msg
      file `shouldBe` "/tmp/main.tnix"
      lookupDocumentText file docs `shouldBe` Just "2"
      fmap analysisBindings result `shouldBe` Right (Map.fromList [("result", Scheme [] tInt)])

    it "keeps the previous cache when incremental edits are invalid" $ do
      let docs = documentsFromList [("/tmp/main.tnix", "x")]
          msg = changeMessage "/tmp/main.tnix" [replaceRange 0 2 0 3 "y"]
      (docs', _, result) <- updateDocuments readNever analyzeStub docs msg
      docs' `shouldBe` docs
      result `shouldBe` Left "content change character is out of bounds"

    it "retains the latest text even when analysis reports an error" $ do
      let msg = openMessage "/tmp/main.tnix" "missing"
      (docs, file, result) <- updateDocuments readNever analyzeFailing mempty msg
      lookupDocumentText file docs `shouldBe` Just "missing"
      result `shouldBe` Left "analysis failed for missing"

  describe "closeDocuments" $ do
    it "removes cached documents and returns the closed path" $ do
      let docs = documentsFromList [("/tmp/main.tnix", "1"), ("/tmp/other.tnix", "2")]
          msg = closeMessage "/tmp/main.tnix"
          (docs', closed) = closeDocuments docs msg
      closed `shouldBe` Just "/tmp/main.tnix"
      docs' `shouldBe` documentsFromList [("/tmp/other.tnix", "2")]

    it "leaves the cache untouched when the close notification is malformed" $ do
      let docs = documentsFromList [("/tmp/main.tnix", "1")]
          (docs', closed) = closeDocuments docs (object ["params" .= object []])
      closed `shouldBe` Nothing
      docs' `shouldBe` docs

  describe "hoverDocument" $ do
    it "prefers cached content over disk when computing hover" $ do
      let docs = documentsFromList [("/tmp/main.tnix", "box")]
      hover <- hoverDocument (\_ -> pure (Left "should not read")) analyzeStub docs (hoverMessage "/tmp/main.tnix" 0 1)
      hoverText hover `shouldBe` "```tnix\nString\n```"

    it "falls back to disk when the document is not cached" $ do
      hover <- hoverDocument (\_ -> pure (Right "1")) analyzeStub mempty (hoverMessage "/tmp/main.tnix" 0 0)
      hoverText hover `shouldBe` "```tnix\nInt\n```"

    it "renders readable hover errors when reading from disk fails" $ do
      hover <- hoverDocument (\_ -> pure (Left "boom")) analyzeStub mempty (hoverMessage "/tmp/main.tnix" 0 0)
      hoverText hover `shouldBe` "```tnix\nboom\n```"

  describe "completionDocument" $ do
    it "returns field completions from the current analyzed buffer" $ do
      completions <- completionDocument readNever analyzeCompletion (documentsFromList [("/tmp/main.tnix", "box.")]) (completionMessage "/tmp/main.tnix" 0 4)
      completionLabels completions `shouldBe` ["alpha", "beta"]

    it "keeps offering completions from the last successful analysis while the buffer is temporarily invalid" $ do
      let file = "/tmp/main.tnix"
          validContent = Text.unlines ["let", "  box = { alpha = 1; beta = \"x\"; };", "in box.alpha"]
          invalidContent = Text.unlines ["let", "  box = { alpha = 1; beta = \"x\"; };", "in box."]
      (validDocs, _, _) <- updateDocuments readNever analyzeStrictCompletion mempty (openMessage file validContent)
      (invalidDocs, _, invalidResult) <- updateDocuments readNever analyzeStrictCompletion validDocs (changeMessage file [object ["text" .= invalidContent]])
      invalidResult `shouldSatisfy` isLeft
      completions <- completionDocument readNever analyzeStrictCompletion invalidDocs (completionMessage file 2 7)
      completionLabels completions `shouldBe` ["alpha", "beta"]

    it "returns an empty completion list when loading the document fails" $ do
      completions <- completionDocument (\_ -> pure (Left "boom")) analyzeCompletion mempty (completionMessage "/tmp/main.tnix" 0 0)
      completionLabels completions `shouldBe` []

  describe "definitionDocument" $ do
    it "jumps to the local binding of the selected symbol" $ do
      let content = Text.unlines ["let", "  value = 1;", "in value"]
      definition <- definitionDocument readNever analyzeCompletion (documentsFromList [("/tmp/main.tnix", content)]) (definitionMessage "/tmp/main.tnix" 2 6)
      definitionLocation definition `shouldBe` Just ("/tmp/main.tnix", 1, 2, 7)

    it "jumps builtins members into the nearest ambient declaration file" $
      withTempTree
        [ ("builtins.d.tnix", Text.unlines ["declare \"builtins\" {", "  map :: Int -> Int;", "};"]),
          ("app/main.tnix", "builtins.map")
        ]
        ( \root -> do
            let file = root </> "app/main.tnix"
            definition <- definitionDocument readFileStub analyzeCompletion mempty (definitionMessage file 0 10)
            definitionLocation definition `shouldBe` Just (root </> "builtins.d.tnix", 1, 2, 5)
        )

  describe "referencesDocument" $
    it "returns all local references for a binding in the active buffer" $ do
      let content = Text.unlines ["let", "  value = 1;", "in value value"]
      references <- referencesDocument readNever analyzeCompletion (documentsFromList [("/tmp/main.tnix", content)]) (definitionMessage "/tmp/main.tnix" 2 6)
      referenceLocations references `shouldBe` [("/tmp/main.tnix", 1, 2, 7), ("/tmp/main.tnix", 2, 3, 8), ("/tmp/main.tnix", 2, 9, 14)]

  describe "renameDocument" $
    it "builds workspace edits for local references" $ do
      let content = Text.unlines ["let", "  value = 1;", "in value"]
      rename <- renameDocument readNever analyzeCompletion (documentsFromList [("/tmp/main.tnix", content)]) (renameMessage "/tmp/main.tnix" 2 6 "answer")
      renameEdits rename `shouldBe` [("/tmp/main.tnix", [(1, 2, 7, "answer"), (2, 3, 8, "answer")])]

  describe "documentSymbolsDocument" $
    it "surfaces aliases and let bindings as symbols" $ do
      let content = Text.unlines ["type Box = { value :: Int; };", "let", "  value = 1;", "in value"]
      symbols <- documentSymbolsDocument readNever analyzeCompletion (documentsFromList [("/tmp/main.tnix", content)]) (documentSymbolMessage "/tmp/main.tnix")
      symbolNames symbols `shouldBe` ["Box", "value"]

  describe "workspaceSymbolsDocument" $
    it "searches symbols across workspace files" $
      withTempTree
        [ ("tnix.config.tnix", Text.unlines ["{", "  name = \"demo\";", "  entry = \"./pkg/a.tnix\";", "}"]),
          ("pkg/a.tnix", Text.unlines ["type Box = { value :: Int; };", "1"]),
          ("pkg/b.tnix", Text.unlines ["let", "  widget = 1;", "in widget"])
        ]
        ( \root -> do
            let docs = documentsFromList [(root </> "pkg/a.tnix", Text.unlines ["type Box = { value :: Int; };", "1"])]
            symbols <- workspaceSymbolsDocument readFileStub analyzeCompletion docs (workspaceSymbolMessage "wid")
            symbolNames symbols `shouldBe` ["widget"]
        )

  describe "codeActionsDocument" $
    it "offers directive quick fixes and obvious rename suggestions" $ do
      let content = "mising"
          diagnostic = object ["message" .= ("unbound name: \"mising\"" :: Text), "range" .= rangeObject 0 0 0 6]
      actions <- codeActionsDocument readNever analyzeCompletion (documentsFromList [("/tmp/main.tnix", content)]) (codeActionMessage "/tmp/main.tnix" [diagnostic])
      codeActionTitles actions `shouldBe` ["Add `# @tnix-ignore`", "Add `# @tnix-expected`", "Replace with `missing`"]

  describe "semanticTokensDocument" $
    it "returns encoded semantic tokens for keywords, types, and strings" $ do
      let content = Text.unlines ["type Box = String;", "let", "  value = \"tnix\";", "in value"]
      tokens <- semanticTokensDocument readNever analyzeCompletion (documentsFromList [("/tmp/main.tnix", content)]) (documentSymbolMessage "/tmp/main.tnix")
      semanticTokenPayload tokens `shouldSatisfy` (not . null)
  where
    readNever _ = expectationFailure "unexpected file read" >> fail "unexpected file read"
    readFileStub = fmap Right . TextIO.readFile
    analyzeStub file content
      | content == "1" = pure (attachProgramFallback file content intAnalysis)
      | content == "box" = pure (attachProgramFallback file content stringBindingAnalysis)
      | otherwise = pure (attachProgramFallback file content defaultAnalysis)
    analyzeCompletion file content = pure (dynamicAnalysis file content)
    analyzeStrictCompletion file content = pure (strictDynamicAnalysis file content)
    analyzeFailing _ content = pure (Left ("analysis failed for " <> showText content))
    showText = Text.unpack

attachProgram :: FilePath -> Text -> Analysis -> Either String Analysis
attachProgram file content analysis = do
  program <- parseText file content
  pure analysis {analysisProgram = program}

attachProgramFallback :: FilePath -> Text -> Analysis -> Either String Analysis
attachProgramFallback file content analysis =
  either
    (const (attachProgram file "1" analysis))
    (\program -> Right analysis {analysisProgram = program})
    (parseText file content)

dynamicAnalysis :: FilePath -> Text -> Either String Analysis
dynamicAnalysis file content = do
  program <- either (const (parseText file "1")) Right (parseText file content)
  pure
    completionAnalysis
      { analysisProgram = program,
        analysisBindings = inferredBindings content program
      }

strictDynamicAnalysis :: FilePath -> Text -> Either String Analysis
strictDynamicAnalysis file content = do
  program <- parseText file content
  pure
    completionAnalysis
      { analysisProgram = program,
        analysisBindings = inferredBindings content program
      }

inferredBindings :: Text -> Program -> Map.Map Text Scheme
inferredBindings content program =
  Map.fromList
    [ (name, schemeForName name)
      | name <- nub (letBindingNames program <> textHints)
    ]
  where
    textHints =
      [ "box" | "box" `Text.isInfixOf` content ]
        <> ["missing" | "mising" `Text.isInfixOf` content]

schemeForName :: Text -> Scheme
schemeForName "box" =
  Scheme
    []
    ( TRecord
        ( Map.fromList
            [ ("alpha", tInt),
              ("beta", tString)
            ]
        )
    )
schemeForName _ = Scheme [] tInt

letBindingNames :: Program -> [Text]
letBindingNames program =
  case programExpr program of
    Just (Marked _ (ELet items _)) ->
      [ name
        | Marked _ (LetBinding name _) <- items
      ]
    _ -> []

intAnalysis :: Analysis
intAnalysis =
  Analysis
    { analysisProgram = error "unused in session tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings = Map.empty,
      analysisAliases = mempty,
      analysisAmbient = mempty
    }

defaultAnalysis :: Analysis
defaultAnalysis =
  Analysis
    { analysisProgram = error "unused in session tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings = Map.fromList [("result", Scheme [] tInt)],
      analysisAliases = mempty,
      analysisAmbient = mempty
    }

stringBindingAnalysis :: Analysis
stringBindingAnalysis =
  Analysis
    { analysisProgram = error "unused in session tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings = Map.fromList [("box", Scheme [] tString)],
      analysisAliases = mempty,
      analysisAmbient = mempty
    }

completionAnalysis :: Analysis
completionAnalysis =
  Analysis
    { analysisProgram = error "unused in session tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings =
        Map.fromList
          [ ( "box",
              Scheme
                []
                ( TRecord
                    ( Map.fromList
                        [ ("alpha", tInt),
                          ("beta", tString)
                        ]
                    )
                )
            ),
            ("value", Scheme [] tInt)
          ],
      analysisAliases = mempty,
      analysisAmbient = mempty
    }

openMessage :: FilePath -> Text -> Value
openMessage file text =
  object
    [ "params"
        .= object
          [ "textDocument"
              .= object
                [ "uri" .= ("file://" <> file),
                  "text" .= text
                ]
          ]
    ]

changeMessage :: FilePath -> [Value] -> Value
changeMessage file changes =
  object
    [ "params"
        .= object
          [ "textDocument" .= object ["uri" .= ("file://" <> file)],
            "contentChanges" .= changes
          ]
    ]

closeMessage :: FilePath -> Value
closeMessage file =
  object
    [ "params"
        .= object
          [ "textDocument" .= object ["uri" .= ("file://" <> file)]
          ]
    ]

hoverMessage :: FilePath -> Int -> Int -> Value
hoverMessage file lineNo charNo =
  object
    [ "params"
        .= object
          [ "textDocument" .= object ["uri" .= ("file://" <> file)],
            "position" .= object ["line" .= lineNo, "character" .= charNo]
          ]
    ]

completionMessage :: FilePath -> Int -> Int -> Value
completionMessage = hoverMessage

definitionMessage :: FilePath -> Int -> Int -> Value
definitionMessage = hoverMessage

documentSymbolMessage :: FilePath -> Value
documentSymbolMessage file =
  object
    [ "params"
        .= object
          [ "textDocument" .= object ["uri" .= ("file://" <> file)]
          ]
    ]

renameMessage :: FilePath -> Int -> Int -> Text -> Value
renameMessage file lineNo charNo newName =
  object
    [ "params"
        .= object
          [ "textDocument" .= object ["uri" .= ("file://" <> file)],
            "position" .= object ["line" .= lineNo, "character" .= charNo],
            "newName" .= newName
          ]
    ]

workspaceSymbolMessage :: Text -> Value
workspaceSymbolMessage query =
  object
    [ "params"
        .= object
          [ "query" .= query
          ]
    ]

codeActionMessage :: FilePath -> [Value] -> Value
codeActionMessage file diagnostics =
  object
    [ "params"
        .= object
          [ "textDocument" .= object ["uri" .= ("file://" <> file)],
            "context" .= object ["diagnostics" .= diagnostics]
          ]
    ]

replaceRange :: Int -> Int -> Int -> Int -> Text -> Value
replaceRange startLine startChar endLine endChar text =
  object
    [ "range"
        .= object
          [ "start" .= object ["line" .= startLine, "character" .= startChar],
            "end" .= object ["line" .= endLine, "character" .= endChar]
          ],
      "text" .= text
    ]

rangeObject :: Int -> Int -> Int -> Int -> Value
rangeObject startLine startChar endLine endChar =
  object
    [ "start" .= object ["line" .= startLine, "character" .= startChar],
      "end" .= object ["line" .= endLine, "character" .= endChar]
    ]

hoverText :: Value -> Text
hoverText value =
  case value of
    Object obj ->
      case KeyMap.lookup "contents" obj of
        Just (Object contents) ->
          case KeyMap.lookup "value" contents of
            Just (String text) -> text
            _ -> "missing hover value"
        _ -> "missing hover contents"
    _ -> "unexpected hover payload"

completionLabels :: Value -> [Text]
completionLabels value =
  case value of
    Object obj ->
      case KeyMap.lookup "items" obj of
        Just (Array items) ->
          [ label
            | Object item <- toList items,
              Just (String label) <- [KeyMap.lookup "label" item]
          ]
        _ -> []
    _ -> []

definitionLocation :: Value -> Maybe (FilePath, Int, Int, Int)
definitionLocation value = do
  Object obj <- pure value
  String uri <- KeyMap.lookup "uri" obj
  Object range <- KeyMap.lookup "range" obj
  Object start <- KeyMap.lookup "start" range
  Object end <- KeyMap.lookup "end" range
  Number lineNo <- KeyMap.lookup "line" start
  Number startChar <- KeyMap.lookup "character" start
  Number endChar <- KeyMap.lookup "character" end
  pure (Text.unpack (Text.drop 7 uri), floor lineNo, floor startChar, floor endChar)

referenceLocations :: Value -> [(FilePath, Int, Int, Int)]
referenceLocations value =
  case value of
    Array items -> mapMaybe definitionLocation (toList items)
    _ -> []

renameEdits :: Value -> [(FilePath, [(Int, Int, Int, Text)])]
renameEdits value =
  case value of
    Object obj ->
      case KeyMap.lookup "changes" obj of
        Just (Object changes) ->
          [ ( Text.unpack (Text.drop 7 uri),
              [ (floor lineNo, floor startChar, floor endChar, newText)
                | Object edit <- toList edits,
                  Just (Object range) <- [KeyMap.lookup "range" edit],
                  Just (Object start) <- [KeyMap.lookup "start" range],
                  Just (Object ending) <- [KeyMap.lookup "end" range],
                  Just (Number lineNo) <- [KeyMap.lookup "line" start],
                  Just (Number startChar) <- [KeyMap.lookup "character" start],
                  Just (Number endChar) <- [KeyMap.lookup "character" ending],
                  Just (String newText) <- [KeyMap.lookup "newText" edit]
              ]
            )
            | (key, Array edits) <- KeyMap.toList changes,
              let uri = Key.toText key
          ]
        _ -> []
    _ -> []

symbolNames :: Value -> [Text]
symbolNames value =
  case value of
    Array items ->
      [ name
        | Object item <- toList items,
          Just (String name) <- [KeyMap.lookup "name" item]
      ]
    _ -> []

codeActionTitles :: Value -> [Text]
codeActionTitles value =
  case value of
    Array items ->
      [ title
        | Object item <- toList items,
          Just (String title) <- [KeyMap.lookup "title" item]
      ]
    _ -> []

semanticTokenPayload :: Value -> [Int]
semanticTokenPayload value =
  case value of
    Object obj ->
      case KeyMap.lookup "data" obj of
        Just (Array items) ->
          [ floor number
            | Number number <- toList items
          ]
        _ -> []
    _ -> []

withTempTree :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withTempTree files action = bracket createRoot removePathForcibly (\root -> writeTree root >> action root)
  where
    createRoot = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp "tnix-lsp-session"
      hClose handle
      removeFile path
      createDirectory path
      pure path
    writeTree root =
      mapM_
        (\(relative, content) -> do
           let path = root </> relative
           createDirectoryIfMissing True (takeDirectory path)
           TextIO.writeFile path content)
        files
