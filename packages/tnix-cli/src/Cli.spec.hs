{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cli
import Control.Exception (bracket)
import Data.Map.Strict qualified as Map
import Data.List (isInfixOf)
import Data.Text qualified as Text
import Driver (Analysis (..))
import Options.Applicative (ParserPrefs, ParserResult (..), defaultPrefs, execParserPure, getParseResult, info, renderFailure)
import System.Directory (createDirectory, doesFileExist, getTemporaryDirectory, removeFile, removePathForcibly)
import System.IO (hClose, openTempFile)
import Test.Hspec
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "commandParser" $ do
    it "parses compile with output" $
      parse ["compile", "main.tnix", "-o", "dist/main.nix"]
        `shouldBe` Just (Compile "main.tnix" (Just "dist/main.nix"))

    it "parses check and emit commands" $ do
      parse ["check", "main.tnix"] `shouldBe` Just (Check "main.tnix")
      parse ["emit", "main.tnix"] `shouldBe` Just (Emit "main.tnix" Nothing)

    it "reports an error for missing subcommands" $ do
      case parserResult [] of
        Failure failure ->
          let (message, _) = renderFailure failure ""
           in message `shouldSatisfy` ("Missing: COMMAND" `isInfixOf`)
        other -> expectationFailure ("expected parse failure, got " <> show other)

  describe "renderAnalysis" $
    it "renders root schemes before named bindings" $ do
      let analysis =
            Analysis
              { analysisProgram = error "unused in cli tests",
                analysisRoot = Just (Scheme [] tInt),
                analysisBindings = Map.fromList [("box", Scheme [] tString), ("id", Scheme ["a"] (TFun (TVar "a") (TVar "a")))]
              }
      renderAnalysis analysis
        `shouldBe` Text.unlines ["root: Int", "box :: String", "id :: forall a. a -> a"]

  describe "writeOutput" $
    it "creates parent directories before writing files" $
      withTempDir $ \root -> do
        let path = root <> "/dist/nested/out.txt"
        writeOutput (Just path) "hello"
        doesFileExist path `shouldReturn` True
  where
    parserInfo = info commandParser mempty
    parserPrefs :: ParserPrefs
    parserPrefs = defaultPrefs
    parse = getParseResult . execParserPure parserPrefs parserInfo
    parserResult = execParserPure parserPrefs parserInfo

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir =
  bracket createRoot removePathForcibly
  where
    createRoot = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp "tnix-cli-spec"
      hClose handle
      removeFile path
      createDirectory path
      pure path
