{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Kind
import Parser (parseProgram)
import Syntax (Program (programAliases))
import Test.Hspec
import TestSupport (expectRight, source)
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "higher-kinded kind inference" $ do
  it "accepts aliases that abstract over unary type constructors" $ do
    kinds <-
      expectRight $
        inferAliasKinds
          [ TypeAlias "Box" ["a"] (TRecord (Map.fromList [("value", TVar "a")])),
            TypeAlias "Apply" ["f", "a"] (TApp (TVar "f") (TVar "a")),
            TypeAlias "Twice" ["f", "a"] (TApp (TVar "f") (TApp (TVar "f") (TVar "a")))
          ]
    inferTypeKind kinds (TApp (TApp (TCon "Apply") (TCon "Box")) tInt)
      `shouldBe` Right KType
    inferTypeKind kinds (TApp (TApp (TCon "Twice") (TCon "List")) tInt)
      `shouldBe` Right KType

  it "accepts aliases that return higher-kinded constructors" $ do
    kinds <-
      expectRight $
        inferAliasKinds
          [ TypeAlias "Id" ["f"] (TVar "f"),
            TypeAlias "Apply" ["f", "a"] (TApp (TVar "f") (TVar "a"))
          ]
    inferTypeKind kinds (TApp (TApp (TCon "Apply") (TApp (TCon "Id") (TCon "List"))) tInt)
      `shouldBe` Right KType

  it "rejects applying concrete types as constructors" $
    inferAliasKinds [TypeAlias "Bad" ["a"] (TApp tInt (TVar "a"))]
      `shouldSatisfy` isLeft

  it "rejects oversaturated constructors" $
    inferTypeKind mempty (TApp (tList tInt) tString)
      `shouldSatisfy` isLeft

  it "validates higher-kinded annotations inside parsed programs" $ do
    program <-
      expectRight $
        parseProgram
          "main.tnix"
          ( source
              [ "type Id f = f;",
                "type Apply f a = f a;",
                "declare \"./lib.nix\" { default :: Apply (Id List) Int; };",
                "let value :: Apply (Id List) Int;",
                "    value = import ./lib.nix;",
                "in value"
              ]
          )
    validateProgramKinds (programAliases program) program
      `shouldSatisfy` isRight
  where
    isLeft (Left _) = True
    isLeft _ = False
    isRight (Right _) = True
    isRight _ = False
