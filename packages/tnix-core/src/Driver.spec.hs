{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Test.Hspec
import Driver (Analysis (..), analyzeFile, analyzeText)
import Pretty (renderScheme)
import TestSupport (expectLeftContaining, expectRight, source, withTempTree)
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "analysis" $ do
  it "infers literal roots" $ do
    analysis <- analyzeText "main.tnix" "1" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] (TLit (LInt 1)))

  it "preserves declared binding schemes while allowing gradual root inference" $ do
    analysis <-
      analyzeText
        "main.tnix"
        ( source
            [ "let",
              "  id :: forall a. a -> a;",
              "  id = x: x;",
              "in id"
            ]
        )
        >>= expectRight
    Map.lookup "id" (analysisBindings analysis)
      `shouldBe` Just (Scheme ["a"] (TFun (TVar "a") (TVar "a")))
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "forall t0. t0 -> t0"

  it "follows structural field selections" $ do
    analysis <- analyzeText "main.tnix" "{ nested = { value = 1; }; }.nested.value" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] (TLit (LInt 1)))

  it "reports missing fields" $
    analyzeText "main.tnix" "{ value = 1; }.missing" >>= (`expectLeftContaining` "missing field")

  it "reports unbound names" $
    analyzeText "main.tnix" "missing" >>= (`expectLeftContaining` "unbound name")

  it "joins list element types instead of forcing eager normalization" $ do
    analysis <- analyzeText "main.tnix" "[1 2]" >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "List (1 | 2)"

  it "uses inline ambient declarations for imports" $ do
    analysis <-
      analyzeText
        "main.tnix"
        ( source
            [ "declare \"./lib.nix\" { default :: { value :: Int; }; };",
              "import ./lib.nix"
            ]
        )
        >>= expectRight
    analysisRoot analysis
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("value", tInt)])))

  it "loads ambient declarations from sibling .d.tnix files" $
    withTempTree
      [ ("app/main.tnix", "import ./lib.nix"),
        ("app/types.d.tnix", "declare \"./lib.nix\" { default :: { value :: Int; label :: String; }; };")
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/app/main.tnix") >>= expectRight
          analysisRoot analysis
            `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("label", tString), ("value", tInt)])))
      )

  it "joins field types when selecting from unions of compatible records" $ do
    analysis <-
      analyzeText
        "main.tnix"
        ( source
            [ "let",
              "  candidate = if true then { value = 1; } else { value = \"x\"; };",
              "in candidate.value"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "1 | \"x\""

  it "rejects duplicate let bindings and signatures" $ do
    analyzeText "main.tnix" (source ["let", "  value = 1;", "  value = 2;", "in value"])
      >>= (`expectLeftContaining` "duplicate bindings")
    analyzeText "main.tnix" (source ["let", "  value :: Int;", "  value :: String;", "  value = 1;", "in value"])
      >>= (`expectLeftContaining` "duplicate signatures")

  it "rejects duplicate attribute names from fields and inherit clauses" $ do
    analyzeText "main.tnix" (source ["let", "  value = 1;", "in { value = 2; inherit value; }"])
      >>= (`expectLeftContaining` "duplicate attribute")

  it "reports parse failures from sibling declaration files" $
    withTempTree
      [ ("app/main.tnix", "import ./lib.nix"),
        ("app/types.d.tnix", "declare \"./lib.nix\" { default :: ; };")
      ]
      (\root -> analyzeFile (root <> "/app/main.tnix") >>= (`expectLeftContaining` "failed to load declaration file"))

  it "rejects executable declaration files while loading ambient support" $
    withTempTree
      [ ("app/main.tnix", "import ./lib.nix"),
        ("app/types.d.tnix", "declare \"./lib.nix\" { default :: Int; }; 1")
      ]
      (\root -> analyzeFile (root <> "/app/main.tnix") >>= (`expectLeftContaining` "must not contain executable expressions"))

  it "rejects duplicate ambient targets across declaration files" $
    withTempTree
      [ ("app/main.tnix", "import ./lib.nix"),
        ("app/a.d.tnix", "declare \"./lib.nix\" { default :: Int; };"),
        ("app/b.d.tnix", "declare \"./lib.nix\" { default :: String; };")
      ]
      (\root -> analyzeFile (root <> "/app/main.tnix") >>= (`expectLeftContaining` "duplicate ambient declarations"))

  it "rejects duplicate entries inside one ambient declaration" $
    analyzeText
      "main.tnix"
      "declare \"./lib.nix\" { value :: Int; value :: String; }; import ./lib.nix"
      >>= (`expectLeftContaining` "duplicate ambient entry")
