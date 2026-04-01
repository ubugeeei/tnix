{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Indexed container helpers for value-shaped tnix types.
--
-- The language stays runtime-free, but the checker can still track exact list
-- shapes by reflecting them into `Vec`, `Matrix`, and `Tensor` types.
module Indexed
  ( inferListType,
    normalizeIndexedType,
    tensorListView,
    tensorView,
    tupleListView,
    tupleView,
    validateProgramIndexedTypes,
  )
where

import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import Alias (collectApps)
import Syntax (AmbientDecl (ambientEntries), AmbientEntry (ambientEntryType), AttrItem (..), Expr (..), LetItem (..), Pattern (..), Program (..))
import Type (LiteralType (..), Name, Type (..), TypeAlias (typeAliasBody), tDynamic, tList)

inferListType :: (Type -> Type -> Type) -> [Type] -> Type
inferListType joinElem members =
  case members of
    [] -> surfaceTensorType [TLit (LInt 0)] tDynamic
    _ ->
      let normalized = normalizeIndexedType <$> members
          views = tensorView <$> normalized
          elemTy = foldl1 joinElem normalized
          lenTy = TLit (LInt (toInteger (length members)))
       in case sequence views >>= foldTensorMembers joinElem of
            Just (shape, baseTy) -> surfaceTensorType (lenTy : shape) baseTy
            Nothing
              | all isNothing views && hasUniformSequenceFamily normalized -> surfaceTensorType [lenTy] elemTy
              | all isNothing views -> tupleType normalized
              | otherwise -> tList elemTy

normalizeIndexedType :: Type -> Type
normalizeIndexedType = \case
  TTypeList items -> TTypeList (normalizeIndexedType <$> items)
  TFun left right -> TFun (normalizeIndexedType left) (normalizeIndexedType right)
  TRecord fields -> TRecord (fmap normalizeIndexedType fields)
  TUnion members -> TUnion (normalizeIndexedType <$> members)
  TApp fun arg ->
    case collectApps (TApp (normalizeIndexedType fun) (normalizeIndexedType arg)) of
      (TCon "Vec", [lenTy, elemTy]) -> canonicalTensorType [lenTy] elemTy
      (TCon "Matrix", [rowsTy, colsTy, elemTy]) -> canonicalTensorType [rowsTy, colsTy] elemTy
      (TCon "Tensor", [TTypeList dims, elemTy]) -> canonicalTensorType dims elemTy
      (headTy, args) -> foldl TApp headTy args
  TForall vars body -> TForall vars (normalizeIndexedType body)
  TConditional actual patternTy yesTy noTy ->
    TConditional
      (normalizeIndexedType actual)
      (normalizeIndexedType patternTy)
      (normalizeIndexedType yesTy)
      (normalizeIndexedType noTy)
  other -> other

tensorView :: Type -> Maybe ([Type], Type)
tensorView ty =
  case collectApps ty of
    (TCon "Vec", [lenTy, elemTy]) -> Just ([lenTy], elemTy)
    (TCon "Matrix", [rowsTy, colsTy, elemTy]) -> Just ([rowsTy, colsTy], elemTy)
    (TCon "Tensor", [TTypeList dims, elemTy]) -> Just (dims, elemTy)
    _ -> Nothing

tupleView :: Type -> Maybe [Type]
tupleView ty =
  case collectApps ty of
    (TCon "Tuple", [TTypeList items]) -> Just items
    _ -> Nothing

tensorListView :: Type -> Maybe Type
tensorListView ty = do
  (dims, elemTy) <- tensorView (normalizeIndexedType ty)
  case dims of
    [] -> Nothing
    _ : rest ->
      Just
        ( tList
            ( normalizeIndexedType
                (if null rest then elemTy else surfaceTensorType rest elemTy)
            )
        )

tupleListView :: Type -> Maybe Type
tupleListView ty = do
  items <- tupleView (normalizeIndexedType ty)
  pure $
    case items of
      [] -> surfaceTensorType [TLit (LInt 0)] tDynamic
      _ -> tList (foldl1 joinTupleItems items)

validateProgramIndexedTypes :: Program -> Either String ()
validateProgramIndexedTypes program =
  traverse_ (validateType "type alias" . typeAliasBody) (programAliases program)
    *> traverse_ validateAmbientDecl (programAmbient program)
    *> traverse_ validateExpr (programExpr program)

canonicalTensorType :: [Type] -> Type -> Type
canonicalTensorType dims elemTy = TApp (TApp (TCon "Tensor") (TTypeList dims)) elemTy

surfaceTensorType :: [Type] -> Type -> Type
surfaceTensorType dims elemTy =
  case dims of
    [lenTy] -> TApp (TApp (TCon "Vec") lenTy) elemTy
    [rowsTy, colsTy] -> TApp (TApp (TApp (TCon "Matrix") rowsTy) colsTy) elemTy
    _ -> canonicalTensorType dims elemTy

tupleType :: [Type] -> Type
tupleType = TApp (TCon "Tuple") . TTypeList

foldTensorMembers :: (Type -> Type -> Type) -> [([Type], Type)] -> Maybe ([Type], Type)
foldTensorMembers joinElem = \case
  [] -> Nothing
  (shape, elemTy) : rest -> go shape elemTy rest
  where
    go shape elemTy pending =
      case pending of
        [] -> Just (shape, elemTy)
        (nextShape, nextElemTy) : rest
          | nextShape == shape -> go shape (joinElem elemTy nextElemTy) rest
          | otherwise -> Nothing

validateAmbientDecl :: AmbientDecl -> Either String ()
validateAmbientDecl ambientDecl = traverse_ (validateType "ambient entry" . ambientEntryType) (ambientEntries ambientDecl)

validateExpr :: Expr -> Either String ()
validateExpr = \case
  ELambda (PVar _ annotation) body -> traverse_ (validateType "term annotation") annotation *> validateExpr body
  EApp fun arg -> validateExpr fun *> validateExpr arg
  ELet items body -> traverse_ validateLetItem items *> validateExpr body
  EAttrSet items -> traverse_ validateAttrItem items
  ESelect base _ -> validateExpr base
  EIf cond yesExpr noExpr -> validateExpr cond *> validateExpr yesExpr *> validateExpr noExpr
  EList items -> traverse_ validateExpr items
  _ -> pure ()

validateLetItem :: LetItem -> Either String ()
validateLetItem = \case
  LetSignature _ ty -> validateType "term annotation" ty
  LetBinding _ expr -> validateExpr expr

validateAttrItem :: AttrItem -> Either String ()
validateAttrItem = \case
  AttrField _ expr -> validateExpr expr
  AttrInherit _ -> pure ()

validateType :: Text -> Type -> Either String ()
validateType label ty =
  case ty of
    TTypeList dims -> traverse_ (validateNatType label "tensor shape") dims
    TFun left right -> validateType label left *> validateType label right
    TRecord fields -> traverse_ (validateType label) fields
    TUnion members -> traverse_ (validateType label) members
    TApp fun arg ->
      case collectApps (TApp fun arg) of
        (TCon "Tuple", [TTypeList items]) ->
          traverse_ (validateType label) items
        (TCon "Vec", [lenTy, elemTy]) ->
          validateNatType label "Vec length" lenTy *> validateType label elemTy
        (TCon "Matrix", [rowsTy, colsTy, elemTy]) ->
          traverse_ (validateNatType label "Matrix dimension") [rowsTy, colsTy] *> validateType label elemTy
        (TCon "Tensor", [shapeTy, elemTy]) ->
          validateShapeType label shapeTy *> validateType label elemTy
        _ -> validateType label fun *> validateType label arg
    TForall _ body -> validateType label body
    TConditional actual patternTy yesTy noTy ->
      traverse_ (validateType label) [actual, patternTy, yesTy, noTy]
    _ -> pure ()

validateShapeType :: Text -> Type -> Either String ()
validateShapeType label = \case
  TTypeList dims -> traverse_ (validateNatType label "tensor shape") dims
  ty | isObviouslyInvalidIndex ty -> Left (renderIndexError label "Tensor shape" ty)
  _ -> pure ()

validateNatType :: Text -> Text -> Type -> Either String ()
validateNatType label site = \case
  TLit (LInt n)
    | n >= 0 -> pure ()
    | otherwise -> Left (renderIndexError label site (TLit (LInt n)))
  ty
    | isObviouslyInvalidIndex ty -> Left (renderIndexError label site ty)
    | otherwise -> pure ()

renderIndexError :: Text -> Text -> Type -> String
renderIndexError label site ty =
  show label
    <> " uses "
    <> show site
    <> " that is not nat-like: "
    <> show ty

isObviouslyInvalidIndex :: Type -> Bool
isObviouslyInvalidIndex = \case
  TLit (LString _) -> True
  TLit (LBool _) -> True
  TTypeList _ -> True
  TFun _ _ -> True
  TRecord _ -> True
  TUnion _ -> True
  TForall _ _ -> True
  TCon name -> isPrimitiveTypeName name
  _ -> False

isPrimitiveTypeName :: Name -> Bool
isPrimitiveTypeName name = name `elem` ["Bool", "Int", "List", "Null", "Path", "String"]

hasUniformSequenceFamily :: [Type] -> Bool
hasUniformSequenceFamily members =
  case mapMaybe sequenceFamily members of
    [] -> False
    family : rest -> all (== family) rest

sequenceFamily :: Type -> Maybe Name
sequenceFamily = \case
  TLit (LInt _) -> Just "Int"
  TLit (LString _) -> Just "String"
  TLit (LBool _) -> Just "Bool"
  TCon name -> Just name
  _ -> Nothing

joinTupleItems :: Type -> Type -> Type
joinTupleItems left right
  | left == right = left
  | otherwise = TUnion [left, right]

traverse_ :: Foldable f => (a -> Either String b) -> f a -> Either String ()
traverse_ step = foldr (\item acc -> step item *> acc) (Right ())
