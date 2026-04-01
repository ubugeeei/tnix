{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis (..))
import Session
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
      Map.lookup file docs `shouldBe` Just "1"
      fmap analysisRoot result `shouldBe` Right (Just (Scheme [] tInt))

    it "applies incremental changes against cached documents" $ do
      let docs = Map.fromList [("/tmp/main.tnix", "let value = 1;\n")]
          msg = changeMessage "/tmp/main.tnix" [replaceRange 0 4 0 9 "result", replaceRange 0 13 0 14 "2"]
      (docs', _, result) <- updateDocuments readNever analyzeStub docs msg
      Map.lookup "/tmp/main.tnix" docs' `shouldBe` Just "let result = 2;\n"
      fmap analysisBindings result `shouldBe` Right (Map.fromList [("result", Scheme [] tInt)])

    it "falls back to disk for uncached incremental updates" $ do
      let msg = changeMessage "/tmp/main.tnix" [replaceRange 0 0 0 1 "2"]
      (docs, file, result) <- updateDocuments (\_ -> pure (Right "1")) analyzeStub mempty msg
      file `shouldBe` "/tmp/main.tnix"
      Map.lookup file docs `shouldBe` Just "2"
      fmap analysisBindings result `shouldBe` Right (Map.fromList [("result", Scheme [] tInt)])

    it "keeps the previous cache when incremental edits are invalid" $ do
      let docs = Map.fromList [("/tmp/main.tnix", "x")]
          msg = changeMessage "/tmp/main.tnix" [replaceRange 0 2 0 3 "y"]
      (docs', _, result) <- updateDocuments readNever analyzeStub docs msg
      docs' `shouldBe` docs
      result `shouldBe` Left "content change character is out of bounds"

    it "retains the latest text even when analysis reports an error" $ do
      let msg = openMessage "/tmp/main.tnix" "missing"
      (docs, file, result) <- updateDocuments readNever analyzeFailing mempty msg
      Map.lookup file docs `shouldBe` Just "missing"
      result `shouldBe` Left "analysis failed for missing"

  describe "closeDocuments" $ do
    it "removes cached documents and returns the closed path" $ do
      let docs = Map.fromList [("/tmp/main.tnix", "1"), ("/tmp/other.tnix", "2")]
          msg = closeMessage "/tmp/main.tnix"
          (docs', closed) = closeDocuments docs msg
      closed `shouldBe` Just "/tmp/main.tnix"
      docs' `shouldBe` Map.fromList [("/tmp/other.tnix", "2")]

    it "leaves the cache untouched when the close notification is malformed" $ do
      let docs = Map.fromList [("/tmp/main.tnix", "1")]
          (docs', closed) = closeDocuments docs (object ["params" .= object []])
      closed `shouldBe` Nothing
      docs' `shouldBe` docs

  describe "hoverDocument" $ do
    it "prefers cached content over disk when computing hover" $ do
      let docs = Map.fromList [("/tmp/main.tnix", "box")]
      hover <- hoverDocument (\_ -> pure (Left "should not read")) analyzeStub docs (hoverMessage "/tmp/main.tnix" 0 1)
      hoverText hover `shouldBe` "```tnix\nString\n```"

    it "falls back to disk when the document is not cached" $ do
      hover <- hoverDocument (\_ -> pure (Right "1")) analyzeStub mempty (hoverMessage "/tmp/main.tnix" 0 0)
      hoverText hover `shouldBe` "```tnix\nInt\n```"

    it "renders readable hover errors when reading from disk fails" $ do
      hover <- hoverDocument (\_ -> pure (Left "boom")) analyzeStub mempty (hoverMessage "/tmp/main.tnix" 0 0)
      hoverText hover `shouldBe` "```tnix\nboom\n```"
  where
    readNever _ = expectationFailure "unexpected file read" >> fail "unexpected file read"
    analyzeStub _ content
      | content == "1" = pure (Right intAnalysis)
      | content == "box" = pure (Right stringBindingAnalysis)
      | otherwise = pure (Right defaultAnalysis)
    analyzeFailing _ content = pure (Left ("analysis failed for " <> showText content))
    showText = Text.unpack

intAnalysis :: Analysis
intAnalysis =
  Analysis
    { analysisProgram = error "unused in session tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings = Map.empty
    }

defaultAnalysis :: Analysis
defaultAnalysis =
  Analysis
    { analysisProgram = error "unused in session tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings = Map.fromList [("result", Scheme [] tInt)]
    }

stringBindingAnalysis :: Analysis
stringBindingAnalysis =
  Analysis
    { analysisProgram = error "unused in session tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings = Map.fromList [("box", Scheme [] tString)]
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
