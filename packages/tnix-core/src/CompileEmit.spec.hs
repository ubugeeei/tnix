{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Hspec
import Driver (compileText, emitText, parseText)
import Syntax
import TestSupport (expectLeftContaining, expectRight, source)
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "compile and emit" $ do
  it "erases type syntax when compiling to nix" $ do
    output <-
      compileText
        "main.tnix"
        ( source
            [ "let",
              "  id :: forall a. a -> a;",
              "  id = x: x;",
              "in id"
            ]
        )
        >>= expectRight
    "::" `Text.isInfixOf` output `shouldBe` False
    "id = x: x;" `Text.isInfixOf` output `shouldBe` True

  it "matches the exact pretty-printed nix output for nested control flow" $ do
    output <-
      compileText
        "main.tnix"
        ( source
            [ "let",
              "  value :: Int;",
              "  value = 1;",
              "in",
              "  if true then { inherit value; mapper = (x :: Int): x; } else { value = 2; }"
            ]
        )
        >>= expectRight
    output
      `shouldBe` Text.stripEnd
        ( source
            [ "let",
              "  value = 1;",
              "in if true",
              "then {",
              "  inherit value;",
              "  mapper = x: x;",
              "}",
              "else {",
              "  value = 2;",
              "}"
            ]
        )

  it "refuses to compile declaration-only sources" $
    compileText "types.d.tnix" "declare \"./lib.nix\" { default :: Int; };" >>= (`expectLeftContaining` "declaration-only")

  it "erases nested annotations while preserving nix control flow and inherit" $ do
    output <-
      compileText
        "main.tnix"
        ( source
            [ "let",
              "  value :: Int;",
              "  value = 1;",
              "in",
              "  if true then { inherit value; mapper = (x :: Int): x; } else { value = 2; }"
            ]
        )
        >>= expectRight
    "::" `Text.isInfixOf` output `shouldBe` False
    "if true" `Text.isInfixOf` output `shouldBe` True
    "then {" `Text.isInfixOf` output `shouldBe` True
    "inherit value;" `Text.isInfixOf` output `shouldBe` True
    "mapper =" `Text.isInfixOf` output `shouldBe` True
    "x:" `Text.isInfixOf` output `shouldBe` True

  it "emits field-wise declarations for attrset roots" $ do
    output <- emitText "main.tnix" "{ name = \"tnix\"; count = 1; }" >>= expectRight
    program <- expectRight (parseDecl "main.d.tnix" output)
    programAmbient program
      `shouldBe`
        [ AmbientDecl
            "./main.nix"
            [ AmbientEntry "count" (TLit (LInt 1)),
              AmbientEntry "name" (TLit (LString "tnix"))
            ]
        ]

  it "matches the exact emitted declaration text for record roots" $ do
    output <- emitText "main.tnix" "{ name = \"tnix\"; count = 1; }" >>= expectRight
    output
      `shouldBe` Text.stripEnd
        ( source
            [ "declare \"./main.nix\" {",
              "  count :: 1;",
              "  name :: \"tnix\";",
              "};"
            ]
        )

  it "compiles float literals without changing their surface form" $ do
    output <- compileText "main.tnix" "1.5" >>= expectRight
    output `shouldBe` "1.5"

  it "emits float literal roots precisely" $ do
    output <- emitText "main.tnix" "1.5" >>= expectRight
    Text.isInfixOf "default :: 1.5;" output `shouldBe` True

  it "emits default for non-record or quantified roots" $ do
    output <-
      emitText
        "main.tnix"
        ( source
            [ "let",
              "  id :: forall a. a -> a;",
              "  id = x: x;",
              "in id"
            ]
        )
        >>= expectRight
    program <- expectRight (parseDecl "main.d.tnix" output)
    programAmbient program
      `shouldBe`
        [ AmbientDecl
            "./main.nix"
            [AmbientEntry "default" (TForall ["t0"] (TFun Many (TVar "t0") (TVar "t0")))]
        ]

  it "matches the exact emitted declaration text for polymorphic defaults" $ do
    output <-
      emitText
        "main.tnix"
        ( source
            [ "let",
              "  id :: forall a. a -> a;",
              "  id = x: x;",
              "in id"
            ]
        )
        >>= expectRight
    output
      `shouldBe` Text.stripEnd
        ( source
            [ "declare \"./main.nix\" {",
              "  default :: forall t0. t0 -> t0;",
              "};"
            ]
        )

  it "preserves aliases when emitting declaration files" $ do
    output <- emitText "main.tnix" "type Box t = { value :: t; }; { boxed = { value = 1; }; }" >>= expectRight
    program <- expectRight (parseDecl "main.d.tnix" output)
    programAliases program `shouldBe` [TypeAlias "Box" ["t"] (TRecord (Map.fromList [("value", TVar "t")]))]

  it "emits indexed container roots with their inferred shapes" $ do
    output <- emitText "main.tnix" "[[1 2] [3 4]]" >>= expectRight
    program <- expectRight (parseDecl "main.d.tnix" output)
    programAmbient program
      `shouldBe`
        [ AmbientDecl
            "./main.nix"
            [ AmbientEntry
                "default"
                (TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 2))) (TUnion [TLit (LInt 1), TLit (LInt 2), TLit (LInt 3), TLit (LInt 4)]))
            ]
        ]

  it "emits tuple roots for heterogeneous list literals" $ do
    output <- emitText "main.tnix" "[1 \"x\"]" >>= expectRight
    program <- expectRight (parseDecl "main.d.tnix" output)
    programAmbient program
      `shouldBe`
        [ AmbientDecl
            "./main.nix"
            [AmbientEntry "default" (TApp (TCon "Tuple") (TTypeList [TLit (LInt 1), TLit (LString "x")]))]
        ]

  it "round-trips higher-rank tensor declarations through the emitter" $ do
    output <- emitText "main.tnix" "[[[1] [2]] [[3] [4]]]" >>= expectRight
    program <- expectRight (parseDecl "main.d.tnix" output)
    programAmbient program
      `shouldBe`
        [ AmbientDecl
            "./main.nix"
            [ AmbientEntry
                "default"
                (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 2), TLit (LInt 1)])) (TUnion [TLit (LInt 1), TLit (LInt 2), TLit (LInt 3), TLit (LInt 4)]))
            ]
        ]

  it "emits linear function arrows in declaration output" $ do
    output <-
      emitText
        "main.tnix"
        ( source
            [ "let",
              "  consume :: Int %1 -> Int;",
              "  consume = x: x;",
              "in consume"
            ]
        )
        >>= expectRight
    Text.isInfixOf "default :: Int %1 -> Int;" output `shouldBe` True

  it "emits numeric validation and unit wrappers in declaration output" $ do
    output <-
      emitText
        "main.tnix"
        ( source
            [ "let",
              "  timeout :: Unit \"ms\" (Range 0 5000 Nat);",
              "  timeout = 2500;",
              "in timeout"
            ]
        )
        >>= expectRight
    Text.isInfixOf "default :: Unit \"ms\" (Range 0 5000 Nat);" output `shouldBe` True

  it "emits bounded indexed annotations without erasing dependent shape constraints" $ do
    output <-
      emitText
        "main.tnix"
        ( source
            [ "let",
              "  grid :: Matrix (Range 1 2 Nat) 2 Int;",
              "  grid = [[1 2] [3 4]];",
              "in grid"
            ]
        )
        >>= expectRight
    Text.isInfixOf "default :: Matrix (Range 1 2 Nat) 2 Int;" output `shouldBe` True

  it "emits float-based numeric validators in declaration output" $ do
    output <-
      emitText
        "main.tnix"
        ( source
            [ "let",
              "  ratio :: Range 0.0 1.0 Float;",
              "  ratio = 0.5;",
              "in ratio"
            ]
        )
        >>= expectRight
    Text.isInfixOf "default :: Range 0.0 1.0 Float;" output `shouldBe` True

  it "emits declarations relative to the source basename" $ do
    output <- emitText "nested/app/main.tnix" "{ value = 1; }" >>= expectRight
    Text.isInfixOf "declare \"./main.nix\"" output `shouldBe` True
  where
    parseDecl = parseText
