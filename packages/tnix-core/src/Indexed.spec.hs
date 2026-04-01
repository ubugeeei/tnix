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

  it "widens ragged nested tensors back to structural lists" $
    inferListType
      (joinTypes mempty)
      [ TApp (TApp (TCon "Vec") (TLit (LInt 1))) tInt,
        TApp (TApp (TCon "Vec") (TLit (LInt 2))) tInt
      ]
      `shouldBe` tList (tList tInt)

  it "treats tensors as nested lists when widened structurally" $
    tensorListView (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 3)])) tInt)
      `shouldBe` Just (tList (normalizeIndexedType (TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt)))

  it "rejects obviously invalid indices inside aliases and annotations" $ do
    program <- expectRight $ parseProgram "main.tnix" "type Bad = Tensor [\"x\"] Int; let xs :: Vec String Int; xs = [1]; in xs"
    validateProgramIndexedTypes program `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
