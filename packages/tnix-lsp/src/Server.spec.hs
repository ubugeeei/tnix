{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Driver (Analysis (..))
import Server
import Test.Hspec
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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

  describe "wordAt" $
    it "extracts identifiers around the cursor including punctuation used by nix names" $ do
      wordAt 0 4 "lib.attr-set" `shouldBe` "lib.attr-set"
      wordAt 1 0 "x\ny" `shouldBe` "y"

  describe "publishDiagnostics" $
    it "emits an empty diagnostics list on success and an error diagnostic on failure" $ do
      publishDiagnostics "/tmp/main.tnix" (Right successAnalysis)
        `shouldBe` object ["uri" .= ("file:///tmp/main.tnix" :: String), "diagnostics" .= ([] :: [Value])]
      case publishDiagnostics "/tmp/main.tnix" (Left "boom") of
        Object obj ->
          KeyMap.lookup "diagnostics" obj `shouldBe` Just (toJSON [diag "boom"])
        other -> expectationFailure ("expected object, got " <> show other)

  describe "hoverResult" $
    it "prefers the hovered symbol type and falls back to the root type" $ do
      hoverResult (Right successAnalysis) "box" 0 1
        `shouldBe` object ["contents" .= object ["kind" .= ("markdown" :: String), "value" .= ("```tnix\nString\n```" :: String)]]
      hoverResult (Right rootOnlyAnalysis) "?" 0 0
        `shouldBe` object ["contents" .= object ["kind" .= ("markdown" :: String), "value" .= ("```tnix\nInt\n```" :: String)]]
      hoverResult (Left "type mismatch") "box" 0 0
        `shouldBe` object ["contents" .= object ["kind" .= ("markdown" :: String), "value" .= ("```tnix\ntype mismatch\n```" :: String)]]
  where
    successAnalysis =
      Analysis
        { analysisProgram = error "unused in server tests",
          analysisRoot = Just (Scheme [] tInt),
          analysisBindings = Map.fromList [("box", Scheme [] tString)]
        }
    rootOnlyAnalysis =
      Analysis
        { analysisProgram = error "unused in server tests",
          analysisRoot = Just (Scheme [] tInt),
          analysisBindings = Map.empty
        }
