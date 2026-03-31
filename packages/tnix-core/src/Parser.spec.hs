{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Test.Hspec
import Parser (parseProgram)
import Syntax
import TestSupport (expectRight, source)
import Type

main :: IO ()
main = hspec spec

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

  it "parses comments, inherit clauses, and list conditionals" $ do
    program <-
      expectRight $
        parseProgram
          "main.tnix"
          ( source
              [ "# line comment",
                "/* block comment */",
                "let",
                "  value = 1;",
                "in { inherit value; nested = [ (if true then 1 else 2) ../lib.nix ]; }"
              ]
          )
    programExpr program
      `shouldBe` Just
        ( ELet
            [LetBinding "value" (EInt 1)]
            ( EAttrSet
                [ AttrInherit ["value"],
                  AttrField
                    "nested"
                    (EList [EIf (EBool True) (EInt 1) (EInt 2), EPath "../lib.nix"])
                ]
            )
        )

  it "treats uppercase type heads as constructors in generic applications" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Use t = Result t;"
    programAliases program `shouldBe` [TypeAlias "Use" ["t"] (TApp (TCon "Result") (TVar "t"))]

  it "parses absolute path imports and nested field selections in applications" $ do
    program <- expectRight $ parseProgram "main.tnix" "(import /etc/hosts).meta.value"
    programExpr program
      `shouldBe` Just (ESelect (EApp (EVar "import") (EPath "/etc/hosts")) ["meta", "value"])

  it "rejects reserved words as identifiers" $
    parseProgram "main.tnix" "let if = 1; in if" `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
