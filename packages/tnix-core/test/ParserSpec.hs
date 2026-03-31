{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec
import TestSupport (expectRight, source)
import Tnix.Parser (parseProgram)
import Tnix.Syntax
import Tnix.Type

spec :: Spec
spec = describe "parseProgram" $ do
  it "parses aliases, ambient declarations, and a root expression" $ do
    program <- expectRight $
      parseProgram "main.tnix" $
        source
          [ "type Box t = { value :: t; };",
            "declare \"./lib.nix\" { default :: Box Int; };",
            "let box = import ./lib.nix; in box.value"
          ]
    programAliases program
      `shouldBe` [TypeAlias "Box" ["t"] (TRecord (Map.fromList [("value", TVar "t")]))]
    programAmbient program
      `shouldBe` [AmbientDecl "./lib.nix" [AmbientEntry "default" (TApp (TCon "Box") tInt)]]
    programExpr program
      `shouldBe` Just (ELet [LetBinding "box" (EApp (EVar "import") (EPath "./lib.nix"))] (ESelect (EVar "box") ["value"]))

  it "parses typed lambdas and nested selections without changing nix shape" $ do
    program <- expectRight $ parseProgram "main.tnix" "{ nested = { value = 1; }; }.nested.value"
    programExpr program
      `shouldBe` Just
        ( ESelect
            (EAttrSet [AttrField "nested" (EAttrSet [AttrField "value" (EInt 1)])])
            ["nested", "value"]
        )

  it "parses conditional types with infer binders" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Elem t = t extends List (infer u) ? u : dynamic;"
    programAliases program
      `shouldBe` [TypeAlias "Elem" ["t"] (TConditional (TVar "t") (TApp (TCon "List") (TInfer "u")) (TVar "u") TDynamic)]

  it "parses typed lambda binders" $ do
    program <- expectRight $ parseProgram "main.tnix" "(x :: Int): x"
    programExpr program `shouldBe` Just (ELambda (PVar "x" (Just tInt)) (EVar "x"))

  it "rejects reserved words as identifiers" $
    parseProgram "main.tnix" "let if = 1; in if" `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
