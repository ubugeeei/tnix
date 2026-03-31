{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Test.Hspec
import TnixAlias
import TnixSubtyping
import TnixType

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "subtyping and type reduction" $ do
  it "treats literal values as subtypes of primitive constructors" $ do
    isSubtype mempty (TLit (LString "tnix")) tString `shouldBe` True
    isSubtype mempty (TLit (LInt 1)) tInt `shouldBe` True
    isSubtype mempty (TLit (LBool False)) tBool `shouldBe` True

  it "supports structural width subtyping for records" $
    isSubtype mempty (TRecord (Map.fromList [("a", tInt), ("b", tString)])) (TRecord (Map.fromList [("a", tInt)]))
      `shouldBe` True

  it "uses contravariant inputs and covariant outputs for functions" $ do
    let broad = TFun tString (TRecord (Map.fromList [("name", tString), ("value", tInt)]))
        narrow = TFun (TLit (LString "x")) (TRecord (Map.fromList [("name", tString)]))
    isSubtype mempty broad narrow `shouldBe` True

  it "reduces generic aliases including HKT-shaped applications" $ do
    let env =
          mkAliasEnv
            [ TypeAlias "Box" ["T"] (TRecord (Map.fromList [("value", TVar "T")])),
              TypeAlias "Apply" ["F", "A"] (TApp (TVar "F") (TVar "A"))
            ]
    resolveType env (TApp (TCon "Box") tInt)
      `shouldBe` TRecord (Map.fromList [("value", tInt)])
    resolveType env (TApp (TApp (TCon "Apply") (TCon "List")) tString)
      `shouldBe` tList tString

  it "evaluates conditional types by pattern matching with infer" $ do
    let elemOf = TConditional (tList tInt) (TApp (TCon "List") (TInfer "U")) (TVar "U") tDynamic
    resolveType mempty elemOf `shouldBe` tInt

  it "joins compatible types and falls back to unions" $ do
    joinTypes mempty (TLit (LString "x")) tString `shouldBe` tString
    joinTypes mempty tInt tString `shouldBe` TUnion [tInt, tString]

  it "treats dynamic as consistent but not as a subtype of concrete types" $ do
    isConsistent mempty tDynamic tString `shouldBe` True
    isSubtype mempty tDynamic tString `shouldBe` False
