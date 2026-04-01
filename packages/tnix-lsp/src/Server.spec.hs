{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Driver (Analysis (..))
import Server
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (Handle, SeekMode (AbsoluteSeek), hClose, hFlush, hSeek, openTempFile)
import Test.Hspec
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "clientCapabilities" $
    it "advertises hover, completion, definition, and incremental sync support" $
      clientCapabilities
        `shouldBe` object
          [ "capabilities"
              .= object
                [ "hoverProvider" .= True,
                  "completionProvider" .= object ["triggerCharacters" .= ["." :: String]],
                  "definitionProvider" .= True,
                  "declarationProvider" .= True,
                  "textDocumentSync" .= object ["openClose" .= True, "change" .= (2 :: Int)]
                ]
          ]

  describe "contentLengthFromHeaders" $ do
    it "finds content length regardless of header order or casing" $
      contentLengthFromHeaders ["Content-Type: application/vscode-jsonrpc; charset=utf-8", "content-length: 42"]
        `shouldBe` Just 42

    it "rejects missing or malformed lengths" $ do
      contentLengthFromHeaders ["X-Test: 1"] `shouldBe` Nothing
      contentLengthFromHeaders ["Content-Length: nope"] `shouldBe` Nothing

  describe "uri helpers" $
    it "round-trips file uris" $ do
      let path = "/tmp/project/main.tnix"
      uriPath (pathUri path) `shouldBe` path

  describe "uri encoding" $ do
    it "percent-encodes spaces and unicode in file uris" $
      pathUri "/tmp/type space/\x578b.tnix"
        `shouldBe` "file:///tmp/type%20space/%E5%9E%8B.tnix"

    it "decodes percent-encoded file uris back to filesystem paths" $
      uriPath "file:///tmp/type%20space/%E5%9E%8B.tnix"
        `shouldBe` "/tmp/type space/\x578b.tnix"

    it "accepts localhost authorities emitted by some LSP clients" $
      uriPath "file://localhost/tmp/main.tnix"
        `shouldBe` "/tmp/main.tnix"

  describe "documentPath" $
    it "extracts the path from textDocument params" $
      documentPath (Just (object ["textDocument" .= object ["uri" .= ("file:///tmp/main.tnix" :: String)]]))
        `shouldBe` Just "/tmp/main.tnix"

  describe "documentPath localhost" $
    it "normalizes localhost authorities in textDocument params" $
      documentPath (Just (object ["textDocument" .= object ["uri" .= ("file://localhost/tmp/main.tnix" :: String)]]))
        `shouldBe` Just "/tmp/main.tnix"

  describe "applyContentChanges" $ do
    it "replaces the whole document when a change omits range information" $
      applyContentChanges "before" (Just (object ["contentChanges" .= [object ["text" .= ("after" :: String)]]]))
        `shouldBe` Right "after"

    it "applies incremental edits in order" $
      applyContentChanges
        "let value = 1;\n"
        ( Just
            ( object
                [ "contentChanges"
                    .= [ object
                           [ "range"
                               .= object
                                 [ "start" .= object ["line" .= (0 :: Int), "character" .= (4 :: Int)],
                                   "end" .= object ["line" .= (0 :: Int), "character" .= (9 :: Int)]
                                 ],
                             "text" .= ("result" :: String)
                           ],
                         object
                           [ "range"
                               .= object
                                 [ "start" .= object ["line" .= (0 :: Int), "character" .= (13 :: Int)],
                                   "end" .= object ["line" .= (0 :: Int), "character" .= (14 :: Int)]
                                 ],
                             "text" .= ("2" :: String)
                           ]
                       ]
                ]
            )
        )
        `shouldBe` Right "let result = 2;\n"

    it "rejects invalid incremental ranges" $
      applyContentChanges
        "x"
        ( Just
            ( object
                [ "contentChanges"
                    .= [ object
                           [ "range"
                               .= object
                                 [ "start" .= object ["line" .= (0 :: Int), "character" .= (2 :: Int)],
                                   "end" .= object ["line" .= (0 :: Int), "character" .= (3 :: Int)]
                                 ],
                             "text" .= ("y" :: String)
                           ]
                       ]
                ]
            )
        )
        `shouldBe` Left "content change character is out of bounds"

  describe "wordAt" $
    it "extracts identifiers around the cursor including punctuation used by nix names" $ do
      wordAt 0 4 "lib.attr-set" `shouldBe` "lib.attr-set"
      wordAt 1 0 "x\ny" `shouldBe` "y"

  describe "publishDiagnostics" $ do
    it "emits an empty diagnostics list on success and an error diagnostic on failure" $ do
      publishDiagnostics "/tmp/main.tnix" (Right successAnalysis)
        `shouldBe` object ["uri" .= ("file:///tmp/main.tnix" :: String), "diagnostics" .= ([] :: [Value])]
      case publishDiagnostics "/tmp/main.tnix" (Left "boom") of
        Object obj ->
          KeyMap.lookup "diagnostics" obj `shouldBe` Just (toJSON [diag "boom"])
        other -> expectationFailure ("expected object, got " <> show other)

    it "builds an explicit clear-diagnostics notification body" $
      clearDiagnostics "/tmp/main.tnix"
        `shouldBe` object ["uri" .= ("file:///tmp/main.tnix" :: String), "diagnostics" .= ([] :: [Value])]

  describe "publishDiagnosticsWithContent" $ do
    it "highlights unbound names using the token span in the current buffer" $
      diagnosticSpan (publishDiagnosticsWithContent "/tmp/main.tnix" "missing" (Left "unbound name: \"missing\""))
        `shouldBe` Just (0, 0, 7)

    it "uses parser coordinates when a parse error includes line and column data" $
      diagnosticSpan
        ( publishDiagnosticsWithContent
            "/tmp/main.tnix"
            "ok\n  broken"
            (Left "/tmp/main.tnix:2:3:\n  |\n2 |   broken\n  |   ^")
        )
        `shouldBe` Just (1, 2, 8)

  describe "hoverResult" $
    it "prefers the hovered symbol type and falls back to the root type" $ do
      hoverResult (Right successAnalysis) "box" 0 1
        `shouldBe` object ["contents" .= object ["kind" .= ("markdown" :: String), "value" .= ("```tnix\nString\n```" :: String)]]
      hoverResult (Right rootOnlyAnalysis) "?" 0 0
        `shouldBe` object ["contents" .= object ["kind" .= ("markdown" :: String), "value" .= ("```tnix\nInt\n```" :: String)]]
      hoverResult (Left "type mismatch") "box" 0 0
        `shouldBe` object ["contents" .= object ["kind" .= ("markdown" :: String), "value" .= ("```tnix\ntype mismatch\n```" :: String)]]

  describe "completionResult" $ do
    it "offers top-level symbols, builtins, and protocol-safe metadata" $
      completionLabels (completionResult (Right completionAnalysis) "b" 0 1)
        `shouldBe` ["box", "builtins"]

    it "offers record fields after a dotted access and filters by prefix" $ do
      completionLabels (completionResult (Right completionAnalysis) "box." 0 4)
        `shouldBe` ["alpha", "beta"]
      completionLabels (completionResult (Right completionAnalysis) "box.al" 0 6)
        `shouldBe` ["alpha"]

  describe "framing" $ do
    it "writes a Content-Length header followed by a JSON body" $
      withTempHandle $ \handle -> do
        send handle (object ["jsonrpc" .= ("2.0" :: String), "method" .= ("initialized" :: String)])
        hFlush handle
        hSeek handle AbsoluteSeek 0
        payload <- BS.hGetContents handle
        let body = snd (BS.breakSubstring "\r\n\r\n" payload)
        "Content-Length: " `BS.isPrefixOf` payload `shouldBe` True
        decodeStrict' (BS.drop 4 body) `shouldBe` Just (object ["jsonrpc" .= ("2.0" :: String), "method" .= ("initialized" :: String)])

    it "reads a framed message payload from a handle" $
      withTempHandle $ \handle -> do
        B8.hPutStr handle "Content-Length: 17\r\nX-Test: 1\r\n\r\n{\"jsonrpc\":\"2.0\"}"
        hFlush handle
        hSeek handle AbsoluteSeek 0
        readMessage handle `shouldReturn` Just (object ["jsonrpc" .= ("2.0" :: String)])

successAnalysis :: Analysis
successAnalysis =
  Analysis
    { analysisProgram = error "unused in server tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings = Map.fromList [("box", Scheme [] tString)],
      analysisAliases = mempty,
      analysisAmbient = mempty
    }

rootOnlyAnalysis :: Analysis
rootOnlyAnalysis =
  Analysis
    { analysisProgram = error "unused in server tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings = Map.empty,
      analysisAliases = mempty,
      analysisAmbient = mempty
    }

completionAnalysis :: Analysis
completionAnalysis =
  Analysis
    { analysisProgram = error "unused in server tests",
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
            )
          ],
      analysisAliases = mempty,
      analysisAmbient =
        Map.fromList
          [ ( "builtins",
              Scheme
                []
                ( TRecord
                    ( Map.fromList
                        [ ( "map",
                            TForall
                              ["a", "b"]
                              (TFun Many (TFun Many (TVar "a") (TVar "b")) (TFun Many (tList (TVar "a")) (tList (TVar "b"))))
                          )
                        ]
                    )
                )
            )
          ]
    }

withTempHandle :: (Handle -> IO a) -> IO a
withTempHandle action = do
  tmp <- getTemporaryDirectory
  (path, handle) <- openTempFile tmp "tnix-lsp-spec"
  result <- action handle
  hClose handle
  removeFile path
  pure result

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

diagnosticSpan :: Value -> Maybe (Int, Int, Int)
diagnosticSpan value = do
  Object obj <- pure value
  Array diagnostics <- KeyMap.lookup "diagnostics" obj
  Object diagnostic <- listToMaybe (toList diagnostics)
  Object range <- KeyMap.lookup "range" diagnostic
  Object start <- KeyMap.lookup "start" range
  Object end <- KeyMap.lookup "end" range
  Number startLine <- KeyMap.lookup "line" start
  Number startChar <- KeyMap.lookup "character" start
  Number endChar <- KeyMap.lookup "character" end
  pure (floor startLine, floor startChar, floor endChar)
