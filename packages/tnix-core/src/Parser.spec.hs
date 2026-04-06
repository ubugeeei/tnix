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
      `shouldBe` Just (plain (ELet [plain (LetBinding "box" (EApp (EVar "import") (EPath "./lib.nix")))] (ESelect (EVar "box") [SelectName "value"])))

  it "parses typed lambdas and nested selections without changing nix shape" $ do
    program <- expectRight $ parseProgram "main.tnix" "{ nested = { value = 1; }; }.nested.value"
    programExpr program
      `shouldBe` Just
        ( plain
            ( ESelect
                (EAttrSet [AttrField "nested" (EAttrSet [AttrField "value" (EInt 1)])])
                [SelectName "nested", SelectName "value"]
            )
        )

  it "parses infix addition inside lambdas and attrsets" $ do
    program <- expectRight $ parseProgram "main.nix" "{ inc = x: x + 1; }"
    programExpr program
      `shouldBe` Just
        ( plain
            ( EAttrSet
                [ AttrField
                    "inc"
                    (ELambda (PVar "x" Nothing) (EAdd (EVar "x") (EInt 1)))
                ]
            )
        )

  it "parses conditional types with infer binders" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Elem t = t extends List (infer u) ? u : dynamic;"
    programAliases program
      `shouldBe` [TypeAlias "Elem" ["t"] (TConditional (TVar "t") (TApp (TCon "List") (TInfer "u")) (TVar "u") TDynamic)]

  it "parses indexed container types and tensor shapes" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Grid t = Matrix 2 3 t; type Cube t = Tensor [2 3 4] t;"
    programAliases program
      `shouldBe`
        [ TypeAlias "Grid" ["t"] (TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 3))) (TVar "t")),
          TypeAlias
            "Cube"
            ["t"]
            (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 3), TLit (LInt 4)])) (TVar "t"))
        ]

  it "parses float literals together with range and unit types" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Timeout = Unit \"ms\" (Range 0 5000 Nat); 1.5"
    programAliases program
      `shouldBe`
        [ TypeAlias
            "Timeout"
            []
            (TApp (TApp (TCon "Unit") (TLit (LString "ms"))) (TApp (TApp (TApp (TCon "Range") (TLit (LInt 0))) (TLit (LInt 5000))) tNat))
        ]
    programExpr program `shouldBe` Just (plain (EFloat 1.5))

  it "parses any and unknown as distinct built-in gradual types" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Loose = any; type Opaque = unknown;"
    programAliases program
      `shouldBe` [TypeAlias "Loose" [] tAny, TypeAlias "Opaque" [] tUnknown]

  it "parses parenthesized refinements and unions inside tensor shapes" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Batch t = Tensor [(Range 0 2 Nat) (1 | 2) 4] t;"
    programAliases program
      `shouldBe`
        [ TypeAlias
            "Batch"
            ["t"]
            ( TApp
                ( TApp
                    (TCon "Tensor")
                    ( TTypeList
                        [ TApp (TApp (TApp (TCon "Range") (TLit (LInt 0))) (TLit (LInt 2))) tNat,
                          TUnion [TLit (LInt 1), TLit (LInt 2)],
                          TLit (LInt 4)
                        ]
                    )
                )
                (TVar "t")
            )
        ]

  it "parses tuple types as type-only heterogeneous sequences" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Pair = Tuple [Int String];"
    programAliases program
      `shouldBe` [TypeAlias "Pair" [] (TApp (TCon "Tuple") (TTypeList [tInt, tString]))]

  it "parses linear function arrows alongside ordinary arrows" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Consume a = a %1 -> a; type Endo a = a -> a;"
    programAliases program
      `shouldBe`
        [ TypeAlias "Consume" ["a"] (TFun One (TVar "a") (TVar "a")),
          TypeAlias "Endo" ["a"] (TFun Many (TVar "a") (TVar "a"))
        ]

  it "parses typed lambda binders" $ do
    program <- expectRight $ parseProgram "main.tnix" "(x :: Int): x"
    programExpr program `shouldBe` Just (plain (ELambda (PVar "x" (Just tInt)) (EVar "x")))

  it "parses attrset lambda binders used by flakes" $ do
    program <- expectRight $ parseProgram "main.tnix" "{ self, nixpkgs, ... }: self"
    programExpr program `shouldBe` Just (plain (ELambda (PAttrSet ["self", "nixpkgs"] True) (EVar "self")))

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
        ( plain
            ( ELet
                [plain (LetBinding "value" (EInt 1))]
                ( EAttrSet
                    [ AttrInherit ["value"],
                      AttrField
                        "nested"
                        (EList [EIf (EBool True) (EInt 1) (EInt 2), EPath "../lib.nix"])
                    ]
                )
            )
        )

  it "treats uppercase type heads as constructors in generic applications" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Use t = Result t;"
    programAliases program `shouldBe` [TypeAlias "Use" ["t"] (TApp (TCon "Result") (TVar "t"))]

  it "parses absolute path imports and nested field selections in applications" $ do
    program <- expectRight $ parseProgram "main.tnix" "(import /etc/hosts).meta.value"
    programExpr program
      `shouldBe` Just (plain (ESelect (EApp (EVar "import") (EPath "/etc/hosts")) [SelectName "meta", SelectName "value"]))

  it "allows reserved keywords in field and selector positions" $ do
    program <- expectRight $ parseProgram "main.tnix" "{ any = 1; }.any"
    programExpr program
      `shouldBe` Just (plain (ESelect (EAttrSet [AttrField "any" (EInt 1)]) [SelectName "any"]))

  it "parses quoted attribute names and dynamic selections" $ do
    quotedProgram <- expectRight $ parseProgram "main.tnix" "{ \"aarch64-darwin\" = 1; }.\"aarch64-darwin\""
    dynamicProgram <- expectRight $ parseProgram "main.tnix" "self.packages.${system}.default"
    quotedProgram
      `shouldSatisfy`
        ( \program ->
            programExpr program
              == Just
                ( plain
                    ( ESelect
                        (EAttrSet [AttrField "aarch64-darwin" (EInt 1)])
                        [SelectName "aarch64-darwin"]
                    )
                )
        )
    dynamicProgram
      `shouldSatisfy`
        ( \program ->
            programExpr program
              == Just
                ( plain
                    ( ESelect
                        (EVar "self")
                        [ SelectName "packages",
                          SelectDynamic (EVar "system"),
                          SelectName "default"
                        ]
                    )
                )
        )

  it "parses indented strings as executable literals" $ do
    program <-
      expectRight $
        parseProgram
          "main.tnix"
          (source ["''", "hello", "world", "''"])
    programExpr program `shouldBe` Just (plain (EString (Indented "\nhello\nworld\n")))

  it "parses as-casts after selections and inside list items" $ do
    castProgram <- expectRight $ parseProgram "main.tnix" "{ as = 1; }.as as Int as Number"
    listProgram <- expectRight $ parseProgram "main.tnix" "[value as Int]"
    programExpr castProgram
      `shouldBe` Just
        ( plain
            ( ECast
                (ECast (ESelect (EAttrSet [AttrField "as" (EInt 1)]) [SelectName "as"]) tInt)
                tNumber
            )
        )
    programExpr listProgram `shouldBe` Just (plain (EList [ECast (EVar "value") tInt]))

  it "attaches tnix diagnostic directives to the next root expression or let item" $ do
    program <-
      expectRight $
        parseProgram
          "main.tnix"
          ( source
              [ "# @tnix-expected",
                "let",
                "  # @tnix-ignore",
                "  value = missing;",
                "in value"
              ]
          )
    programExpr program
      `shouldBe` Just
        ( Marked
            (Just TnixExpected)
            (ELet [Marked (Just TnixIgnore) (LetBinding "value" (EVar "missing"))] (EVar "value"))
        )

  it "rejects dangling tnix directives without a following line of code" $
    parseProgram "main.tnix" "# @tnix-ignore" `shouldSatisfy` isLeft

  it "rejects reserved words as identifiers" $
    parseProgram "main.tnix" "let if = 1; in if" `shouldSatisfy` isLeft
  where
    plain = Marked Nothing
    isLeft (Left _) = True
    isLeft _ = False
