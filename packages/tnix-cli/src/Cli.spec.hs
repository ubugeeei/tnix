{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Driver (Analysis (..))
import Cli (Command (..), commandOutputPath, commandParser, executeCommand, renderAnalysis, writeOutput)
import Options.Applicative (ParserPrefs, ParserResult (..), defaultPrefs, execParserPure, getParseResult, info, renderFailure)
import System.Directory (createDirectory, createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
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

  describe "commandOutputPath" $
    it "tracks explicit destinations only for write commands" $ do
      commandOutputPath (Compile "main.tnix" (Just "dist/main.nix")) `shouldBe` Just "dist/main.nix"
      commandOutputPath (Emit "main.tnix" (Just "types/main.d.tnix")) `shouldBe` Just "types/main.d.tnix"
      commandOutputPath (Check "main.tnix") `shouldBe` Nothing

  describe "executeCommand" $ do
    it "compiles source files through the driver end-to-end" $
      withTempTree
        [ ( "main.tnix",
            source
              [ "let",
                "  value :: Int;",
                "  value = 1;",
                "in value"
              ]
          )
        ]
        ( \root -> do
            output <- executeCommand (Compile (root <> "/main.tnix") Nothing) >>= expectRight
            output `shouldBe` Text.stripEnd (source ["let", "  value = 1;", "in value"])
        )

    it "renders check output for analyzed files" $
      withTempTree
        [ ( "main.tnix",
            source
              [ "let",
                "  id :: forall a. a -> a;",
                "  id = x: x;",
                "in id"
              ]
          )
        ]
        ( \root -> do
            output <- executeCommand (Check (root <> "/main.tnix")) >>= expectRight
            Text.lines output `shouldBe` ["root: forall t0. t0 -> t0", "id :: forall a. a -> a"]
        )

    it "emits declaration files through the driver end-to-end" $
      withTempTree [("main.tnix", "{ value = 1; }")] $
        \root -> do
          output <- executeCommand (Emit (root <> "/main.tnix") Nothing) >>= expectRight
          output
            `shouldBe` Text.stripEnd
              (source ["declare \"./main.nix\" {", "  value :: 1;", "};"])

    it "surfaces driver failures without writing partial output" $
      withTempTree [("types.d.tnix", "declare \"./lib.nix\" { default :: Int; };")] $
        \root ->
          executeCommand (Compile (root <> "/types.d.tnix") Nothing)
            >>= (`expectLeftContaining` "declaration-only")

  describe "writeOutput" $
    it "creates parent directories before writing files" $
      withTempTree [] $ \root -> do
        let path = root <> "/dist/nested/out.txt"
        writeOutput (Just path) "hello"
        doesFileExist path `shouldReturn` True
  where
    parserInfo = info commandParser mempty
    parserPrefs :: ParserPrefs
    parserPrefs = defaultPrefs
    parse = getParseResult . execParserPure parserPrefs parserInfo
    parserResult = execParserPure parserPrefs parserInfo

expectRight :: Show e => Either e a -> IO a
expectRight (Right value) = pure value
expectRight (Left err) = expectationFailure ("expected Right, got Left: " <> show err) >> fail "expected Right"

expectLeftContaining :: Either String a -> String -> Expectation
expectLeftContaining result needle =
  case result of
    Left err | Text.pack needle `Text.isInfixOf` Text.pack err -> pure ()
    Left err -> expectationFailure ("expected error containing " <> show needle <> ", got " <> show err)
    Right _ -> expectationFailure ("expected Left containing " <> show needle <> ", got Right")

source :: [Text] -> Text
source = Text.unlines

withTempTree :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withTempTree files action = bracket createRoot removePathForcibly (\root -> writeTree root >> action root)
  where
    createRoot = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp "tnix-cli-spec"
      hClose handle
      removeFile path
      createDirectory path
      pure path
    writeTree root =
      forM_ files $ \(relative, content) -> do
        let path = root </> relative
        createDirectoryIfMissing True (takeDirectory path)
        TextIO.writeFile path content
