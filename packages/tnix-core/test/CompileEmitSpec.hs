{-# LANGUAGE OverloadedStrings #-}

module CompileEmitSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Hspec
import TestSupport (expectLeftContaining, expectRight, source)
import Tnix.Driver
import Tnix.Syntax
import Tnix.Type

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

  it "refuses to compile declaration-only sources" $
    compileText "types.d.tnix" "declare \"./lib.nix\" { default :: Int; };" >>= (`expectLeftContaining` "declaration-only")

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
            [AmbientEntry "default" (TForall ["t0"] (TFun (TVar "t0") (TVar "t0")))]
        ]

  it "preserves aliases when emitting declaration files" $ do
    output <- emitText "main.tnix" "type Box t = { value :: t; }; { boxed = { value = 1; }; }" >>= expectRight
    program <- expectRight (parseDecl "main.d.tnix" output)
    programAliases program `shouldBe` [TypeAlias "Box" ["t"] (TRecord (Map.fromList [("value", TVar "t")]))]
  where
    parseDecl = Tnix.Driver.parseText
