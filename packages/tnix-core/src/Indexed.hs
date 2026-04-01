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

import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Alias (collectApps)
import Syntax (AmbientDecl (ambientEntries), AmbientEntry (ambientEntryType), AttrItem (..), Expr (..), LetItem (..), Pattern (..), Program (..))
import Type (LiteralType (..), Name, Type (..), TypeAlias (typeAliasBody), tDynamic, tFloat, tInt, tList, tNat, tNumber)

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
  TFun mult left right -> TFun mult (normalizeIndexedType left) (normalizeIndexedType right)
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
    TFun _ left right -> validateType label left *> validateType label right
    TRecord fields -> traverse_ (validateType label) fields
    TUnion members -> traverse_ (validateType label) members
    TApp fun arg ->
      case collectApps (TApp fun arg) of
        (TCon "Tuple", [TTypeList items]) ->
          traverse_ (validateType label) items
        (TCon "Range", [lowerTy, upperTy, baseTy]) ->
          validateRangeType label lowerTy upperTy baseTy
        (TCon "Unit", [unitTy, baseTy]) ->
          validateUnitType label unitTy baseTy
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
  TCon "Nat" -> pure ()
  TUnion members -> traverse_ (validateNatType label site) members
  ty
    | Just (lowerTy, upperTy, baseTy) <- rangeView ty ->
        validateNatRangeType label site lowerTy upperTy baseTy
  ty
    | isObviouslyInvalidIndex ty -> Left (renderIndexError label site ty)
    | otherwise -> pure ()

validateRangeType :: Text -> Type -> Type -> Type -> Either String ()
validateRangeType label lowerTy upperTy baseTy = do
  lower <- validateNumericBoundType label "Range lower bound" lowerTy
  upper <- validateNumericBoundType label "Range upper bound" upperTy
  validateNumericCarrierType label "Range base" baseTy
  validateRangeBounds label lower upper baseTy

validateUnitType :: Text -> Type -> Type -> Either String ()
validateUnitType label unitTy baseTy = do
  validateUnitLabelType label unitTy
  validateType label baseTy

validateNatRangeType :: Text -> Text -> Type -> Type -> Type -> Either String ()
validateNatRangeType label site lowerTy upperTy baseTy = do
  lower <- validateNumericBoundType label "nat-like lower bound" lowerTy
  upper <- validateNumericBoundType label "nat-like upper bound" upperTy
  case baseTy of
    ty | ty == tInt || ty == tNat -> pure ()
    _ -> Left (renderIndexError label site (TApp (TApp (TApp (TCon "Range") lowerTy) upperTy) baseTy))
  case (lower, upper) of
    (NumericInt low, NumericInt high)
      | low < 0 || high < 0 -> Left (renderIndexError label site (TApp (TApp (TApp (TCon "Range") lowerTy) upperTy) baseTy))
      | low <= high -> pure ()
      | otherwise -> Left (renderRangeError label "Range bounds are inverted" lowerTy upperTy)
    _ -> Left (renderIndexError label site (TApp (TApp (TApp (TCon "Range") lowerTy) upperTy) baseTy))

validateNumericBoundType :: Text -> Text -> Type -> Either String NumericBound
validateNumericBoundType label site = \case
  TLit (LInt n) -> pure (NumericInt n)
  TLit (LFloat n) -> pure (NumericFloat n)
  ty -> Left (renderRangeSiteError label site ty)

validateNumericCarrierType :: Text -> Text -> Type -> Either String ()
validateNumericCarrierType label site = \case
  ty
    | ty == tInt || ty == tFloat || ty == tNumber || ty == tNat -> pure ()
  other -> Left (renderRangeSiteError label site other)

validateRangeBounds :: Text -> NumericBound -> NumericBound -> Type -> Either String ()
validateRangeBounds label lower upper baseTy = do
  when (not (boundsFitBase lower upper baseTy)) $
    Left (renderRangeError label "Range bounds do not match base type" (boundType lower) (boundType upper))
  when (compareNumericBound lower upper == GT) $
    Left (renderRangeError label "Range bounds are inverted" (boundType lower) (boundType upper))

validateUnitLabelType :: Text -> Type -> Either String ()
validateUnitLabelType label = \case
  TLit (LString _) -> pure ()
  other -> Left (renderRangeSiteError label "Unit label" other)

renderIndexError :: Text -> Text -> Type -> String
renderIndexError label site ty =
  show label
    <> " uses "
    <> show site
    <> " that is not nat-like: "
    <> show ty

renderRangeSiteError :: Text -> Text -> Type -> String
renderRangeSiteError label site ty =
  show label
    <> " uses "
    <> show site
    <> " that is not numeric/unit-like enough: "
    <> show ty

renderRangeError :: Text -> String -> Type -> Type -> String
renderRangeError label message leftTy rightTy =
  show label
    <> " has invalid numeric validation: "
    <> message
    <> " ("
    <> show leftTy
    <> ", "
    <> show rightTy
    <> ")"

isObviouslyInvalidIndex :: Type -> Bool
isObviouslyInvalidIndex = \case
  TLit (LString _) -> True
  TLit (LFloat _) -> True
  TLit (LBool _) -> True
  TTypeList _ -> True
  TFun _ _ _ -> True
  TRecord _ -> True
  TForall _ _ -> True
  TCon name -> isPrimitiveTypeName name
  ty
    | Just (TCon "Unit", _) <- pure (collectApps ty) -> True
  _ -> False

isPrimitiveTypeName :: Name -> Bool
isPrimitiveTypeName name = name `elem` ["Bool", "Float", "Int", "List", "Null", "Number", "Path", "String"]

hasUniformSequenceFamily :: [Type] -> Bool
hasUniformSequenceFamily members =
  case traverse sequenceFamily members of
    Nothing -> False
    Just [] -> False
    Just (family : rest) -> all (== family) rest

sequenceFamily :: Type -> Maybe Name
sequenceFamily = \case
  TLit (LFloat _) -> Just "Number"
  TLit (LInt _) -> Just "Number"
  TLit (LString _) -> Just "String"
  TLit (LBool _) -> Just "Bool"
  TCon "Float" -> Just "Number"
  TCon "Int" -> Just "Number"
  TCon "Nat" -> Just "Number"
  TCon "Number" -> Just "Number"
  TCon name -> Just name
  _ -> Nothing

rangeView :: Type -> Maybe (Type, Type, Type)
rangeView ty =
  case collectApps ty of
    (TCon "Range", [lowerTy, upperTy, baseTy]) -> Just (lowerTy, upperTy, baseTy)
    _ -> Nothing

data NumericBound
  = NumericInt Integer
  | NumericFloat Double
  deriving (Eq, Ord, Show)

boundType :: NumericBound -> Type
boundType = \case
  NumericInt n -> TLit (LInt n)
  NumericFloat n -> TLit (LFloat n)

boundsFitBase :: NumericBound -> NumericBound -> Type -> Bool
boundsFitBase lower upper baseTy
  | baseTy == tInt = all isIntegerBound [lower, upper]
  | baseTy == tNat = all isIntegerBound [lower, upper]
  | baseTy == tFloat = True
  | baseTy == tNumber = True
  | otherwise = False

isIntegerBound :: NumericBound -> Bool
isIntegerBound = \case
  NumericInt _ -> True
  NumericFloat _ -> False

compareNumericBound :: NumericBound -> NumericBound -> Ordering
compareNumericBound left right =
  compare (toDouble left) (toDouble right)
  where
    toDouble = \case
      NumericInt n -> fromInteger n
      NumericFloat n -> n

joinTupleItems :: Type -> Type -> Type
joinTupleItems left right
  | left == right = left
  | otherwise = TUnion [left, right]

traverse_ :: Foldable f => (a -> Either String b) -> f a -> Either String ()
traverse_ step = foldr (\item acc -> step item *> acc) (Right ())
