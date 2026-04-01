{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Indexed
import Parser (parseProgram)
import Subtyping (joinTypes)
import Test.Hspec
import TestSupport (expectRight)
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "indexed containers" $ do
  it "normalizes Vec and Matrix into canonical tensor shapes" $ do
    normalizeIndexedType (TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt)
      `shouldBe` TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 3)])) tInt
    normalizeIndexedType (TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 4))) tString)
      `shouldBe` TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 4)])) tString

  it "infers exact vector, matrix, and tensor shapes from list members" $ do
    inferListType (joinTypes mempty) [TLit (LInt 1), TLit (LInt 2)]
      `shouldBe` TApp (TApp (TCon "Vec") (TLit (LInt 2))) (TUnion [TLit (LInt 1), TLit (LInt 2)])
    inferListType
      (joinTypes mempty)
      [ TApp (TApp (TCon "Vec") (TLit (LInt 2))) tInt,
        TApp (TApp (TCon "Vec") (TLit (LInt 2))) tInt
      ]
      `shouldBe` TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 2))) tInt

  it "infers heterogeneous lists as tuples" $
    inferListType (joinTypes mempty) [TLit (LInt 1), TLit (LString "x")]
      `shouldBe` TApp (TCon "Tuple") (TTypeList [TLit (LInt 1), TLit (LString "x")])

  it "preserves ragged nested tensors as vectors with dependent length unions" $
    inferListType
      (joinTypes mempty)
      [ TApp (TApp (TCon "Vec") (TLit (LInt 1))) tInt,
        TApp (TApp (TCon "Vec") (TLit (LInt 2))) tInt
      ]
      `shouldBe` tList (TApp (TApp (TCon "Vec") (TUnion [TLit (LInt 1), TLit (LInt 2)])) tInt)

  it "treats tensors as nested lists when widened structurally" $
    tensorListView (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 3)])) tInt)
      `shouldBe` Just (tList (normalizeIndexedType (TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt)))

  it "widens tuples to structural lists through joined element types" $
    tupleListView (TApp (TCon "Tuple") (TTypeList [TLit (LInt 1), TLit (LString "x")]))
      `shouldBe` Just (tList (TUnion [TLit (LInt 1), TLit (LString "x")]))

  it "rejects obviously invalid indices inside aliases and annotations" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Bad = Tensor [\"x\"] Int; let xs :: Vec String Int; xs = [1]; in xs"
    validateProgramIndexedTypes program `shouldSatisfy` isLeft

  it "accepts nat-like unions and bounded lengths in indexed containers" $ do
    program <-
      expectRight $
        parseProgram
          "main.tnix"
          "type Batch t = Vec (2 | 3 | Range 4 8 Nat) t; let xs :: Vec (Range 2 4 Nat) Int; xs = [1 2 3]; in xs"
    validateProgramIndexedTypes program `shouldBe` Right ()

  it "accepts bounded matrix and tensor dimensions when every axis stays nat-like" $ do
    program <-
      expectRight $
        parseProgram
          "main.tnix"
          "type Grid t = Matrix (Range 1 2 Nat) (2 | 3) t; type Cube t = Tensor [2 (Range 1 2 Nat) 1] t; let grid :: Grid Int; grid = [[1 2] [3 4]]; in grid"
    validateProgramIndexedTypes program `shouldBe` Right ()

  it "rejects non-nat ranges and malformed unit validators" $ do
    natRangeProgram <-
      expectRight $
        parseProgram
          "main.tnix"
          "let xs :: Vec (Range 0.0 2.0 Nat) Int; xs = [1 2]; in xs"
    invertedRangeProgram <-
      expectRight $
        parseProgram
          "main.tnix"
          "let xs :: Vec (Range 4 2 Nat) Int; xs = [1 2]; in xs"
    unitProgram <-
      expectRight $
        parseProgram
          "main.tnix"
          "type Bad = Unit 1 Int; let timeout :: Unit \"ms\" (Range 0 10 String); timeout = 1; in timeout"
    unitShapeProgram <-
      expectRight $
        parseProgram
          "main.tnix"
          "let xs :: Tensor [Unit \"ms\" Nat] Int; xs = [[1]]; in xs"
    validateProgramIndexedTypes natRangeProgram `shouldSatisfy` isLeft
    validateProgramIndexedTypes invertedRangeProgram `shouldSatisfy` isLeft
    validateProgramIndexedTypes unitProgram `shouldSatisfy` isLeft
    validateProgramIndexedTypes unitShapeProgram `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
