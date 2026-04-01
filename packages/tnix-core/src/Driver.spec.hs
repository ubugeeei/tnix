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

  it "infers exact vector roots while preserving precise element unions" $ do
    analysis <- analyzeText "main.tnix" "[1 2]" >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Vec 2 (1 | 2)"

  it "infers empty lists as zero-length vectors" $ do
    analysis <- analyzeText "main.tnix" "[]" >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Vec 0 dynamic"

  it "infers matrix and tensor roots from nested list literals" $ do
    matrixAnalysis <- analyzeText "main.tnix" "[[1 2] [3 4]]" >>= expectRight
    tensorAnalysis <- analyzeText "main.tnix" "[[[1] [2]] [[3] [4]]]" >>= expectRight
    fmap renderScheme (analysisRoot matrixAnalysis) `shouldBe` Just "Matrix 2 2 (1 | 2 | 3 | 4)"
    fmap renderScheme (analysisRoot tensorAnalysis) `shouldBe` Just "Tensor [ 2 2 1 ] (1 | 2 | 3 | 4)"

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

  it "builds record schemes from named ambient exports" $ do
    analysis <-
      analyzeText
        "main.tnix"
        ( source
            [ "declare \"./lib.nix\" { value :: Int; label :: String; };",
              "import ./lib.nix"
            ]
        )
        >>= expectRight
    analysisRoot analysis
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("label", tString), ("value", tInt)])))

  it "supports higher-kinded aliases in ambient declarations and signatures" $ do
    analysis <-
      analyzeText
        "main.tnix"
        ( source
            [ "type Id f = f;",
              "type Apply f a = f a;",
              "declare \"./lib.nix\" { default :: Apply (Id List) Int; };",
              "let",
              "  value :: Apply (Id List) Int;",
              "  value = import ./lib.nix;",
              "in value"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Apply (Id List) Int"

  it "checks Vec, Matrix, and Tensor annotations against list literals" $ do
    vectorAnalysis <-
      analyzeText "main.tnix" (source ["let xs :: Vec 3 Int;", "    xs = [1 2 3];", "in xs"])
        >>= expectRight
    matrixAnalysis <-
      analyzeText "main.tnix" (source ["let grid :: Matrix 2 2 Int;", "    grid = [[1 2] [3 4]];", "in grid"])
        >>= expectRight
    tensorAnalysis <-
      analyzeText
        "main.tnix"
        (source ["let cube :: Tensor [2 2 1] Int;", "    cube = [[[1] [2]] [[3] [4]]];", "in cube"])
        >>= expectRight
    fmap renderScheme (analysisRoot vectorAnalysis) `shouldBe` Just "Vec 3 Int"
    fmap renderScheme (analysisRoot matrixAnalysis) `shouldBe` Just "Matrix 2 2 Int"
    analysisRoot tensorAnalysis
      `shouldBe`
        Just
          ( Scheme
              []
              (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 2), TLit (LInt 1)])) tInt)
          )

  it "treats imports without declarations as dynamic for incremental adoption" $ do
    analysis <- analyzeText "main.tnix" "import ./unknown.nix" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] tDynamic)

  it "loads ambient declarations from sibling .d.tnix files" $
    withTempTree
      [ ("app/main.tnix", "import ./lib.nix"),
        ("app/flake.nix", "{}"),
        ("app/types.d.tnix", "declare \"./lib.nix\" { default :: { value :: Int; label :: String; }; };")
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/app/main.tnix") >>= expectRight
          analysisRoot analysis
            `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("label", tString), ("value", tInt)])))
      )

  it "loads ambient declarations from the workspace root for nested source files" $
    withTempTree
      [ ("flake.nix", "{}"),
        ("types.d.tnix", "declare \"./lib.nix\" { default :: { value :: Int; }; };"),
        ("app/nested/main.tnix", "import ../../lib.nix")
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/app/nested/main.tnix") >>= expectRight
          analysisRoot analysis
            `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("value", tInt)])))
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

  it "rejects passing concrete types to higher-kinded parameters" $
    analyzeText
      "main.tnix"
      ( source
          [ "type Twice f a = f (f a);",
            "let",
            "  bad :: Twice Int String;",
            "  bad = 1;",
            "in bad"
          ]
      )
      >>= (`expectLeftContaining` "kind mismatch")

  it "rejects indexed annotations with invalid dimensions and mismatched shapes" $ do
    analyzeText "main.tnix" "let xs :: Vec \"wide\" Int; xs = [1]; in xs"
      >>= (`expectLeftContaining` "nat-like")
    analyzeText "main.tnix" "let xs :: Vec 2 Int; xs = [1 2 3]; in xs"
      >>= (`expectLeftContaining` "type mismatch")

  it "rejects missing bindings and explicit signature mismatches" $ do
    analyzeText "main.tnix" (source ["let", "  value :: Int;", "in 1"])
      >>= (`expectLeftContaining` "missing bindings for signatures")
    analyzeText "main.tnix" (source ["let", "  value :: String;", "  value = 1;", "in value"])
      >>= (`expectLeftContaining` "type mismatch")

  it "rejects duplicate attribute names from fields and inherit clauses" $ do
    analyzeText "main.tnix" (source ["let", "  value = 1;", "in { value = 2; inherit value; }"])
      >>= (`expectLeftContaining` "duplicate attribute")

  it "reports occurs checks for self-application" $
    analyzeText "main.tnix" "let omega = x: x x; in omega" >>= (`expectLeftContaining` "occurs check failed")

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
        ("app/flake.nix", "{}"),
        ("app/a.d.tnix", "declare \"./lib.nix\" { default :: Int; };"),
        ("app/b.d.tnix", "declare \"./lib.nix\" { default :: String; };")
      ]
      (\root -> analyzeFile (root <> "/app/main.tnix") >>= (`expectLeftContaining` "duplicate ambient declarations"))

  it "rejects duplicate entries inside one ambient declaration" $
    analyzeText
      "main.tnix"
      "declare \"./lib.nix\" { value :: Int; value :: String; }; import ./lib.nix"
      >>= (`expectLeftContaining` "duplicate ambient entry")

  it "prefers inline ambient declarations over workspace declarations" $
    withTempTree
      [ ("flake.nix", "{}"),
        ("types.d.tnix", "declare \"./lib.nix\" { default :: String; };"),
        ( "app/main.tnix",
          source
            [ "declare \"../lib.nix\" { default :: Int; };",
              "import ../lib.nix"
            ]
        )
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/app/main.tnix") >>= expectRight
          analysisRoot analysis `shouldBe` Just (Scheme [] tInt)
      )
