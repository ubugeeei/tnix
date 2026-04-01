{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Structural subtyping, consistency, and type reduction.
--
-- tnix prioritizes incremental adoption over maximum strictness. This module is
-- where that policy becomes concrete: `dynamic` participates through
-- consistency, records use width subtyping, and conditional types are reduced
-- structurally.
module Subtyping
  ( isConsistent,
    isSubtype,
    joinTypes,
    lookupRecordField,
    resolveType,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Alias
import Indexed
import Type

-- | Reduce aliases, erase top-level `forall`, and evaluate conditional types.
--
-- Most higher-level algorithms call this before comparing types so they see a
-- normalized structural view instead of the user-written surface form.
resolveType :: AliasEnv -> Type -> Type
resolveType env = go 0 . normalizeIndexedType . expandAliases env . eraseForall
  where
    go :: Int -> Type -> Type
    go depth ty
      | depth > 32 = ty
      | otherwise =
          case normalizeIndexedType (expandAliases env ty) of
            TTypeList items -> TTypeList (map (go (depth + 1)) items)
            TFun mult a b -> TFun mult (go (depth + 1) a) (go (depth + 1) b)
            TRecord fields -> TRecord (fmap (go (depth + 1)) fields)
            TUnion members -> flattenUnion (TUnion (map (go (depth + 1)) members))
            TApp f x -> TApp (go (depth + 1) f) (go (depth + 1) x)
            TForall vars body -> TForall vars (go (depth + 1) body)
            TConditional a b c d ->
              case matchPattern (go (depth + 1) a) (go (depth + 1) b) of
                Just subst -> go (depth + 1) (substituteTypeVars subst c)
                Nothing ->
                  if isSubtype env a b || isConsistent env a b
                    then go (depth + 1) c
                    else go (depth + 1) d
            other -> other

-- | Resolve a field selection against a record-like type.
--
-- Union members are searched left-to-right and the first matching field is
-- returned. This is intentionally permissive to preserve gradual adoption.
lookupRecordField :: AliasEnv -> Type -> Name -> Maybe Type
lookupRecordField env ty field =
  case resolveType env ty of
    TRecord fields -> Map.lookup field fields
    TUnion members ->
      let hits = mapMaybe (\member -> lookupRecordField env member field) members
       in case hits of
            [] -> Nothing
            _
              | length hits == length members ->
                  Just (foldr1 (joinTypes env) hits)
              | otherwise -> Nothing
    _ -> Nothing

-- | Compute the least-upper-bound style merge used by the checker.
--
-- Where possible this returns an existing supertype; otherwise it constructs a
-- normalized union.
joinTypes :: AliasEnv -> Type -> Type -> Type
joinTypes env left right =
  case (unitView left', unitView right') of
    (Just (leftUnit, leftBase), Just (rightUnit, rightBase))
      | leftUnit == rightUnit ->
          TApp (TApp (TCon "Unit") leftUnit) (joinTypes env leftBase rightBase)
    _ ->
      case (tupleView left', tupleView right') of
        (Just leftItems, Just rightItems)
          | length leftItems == length rightItems ->
              TApp (TCon "Tuple") (TTypeList (zipWith (joinTypes env) leftItems rightItems))
        _ ->
          case (tensorView left', tensorView right') of
            (Just (leftShape, leftElem), Just (rightShape, rightElem))
              | length leftShape == length rightShape ->
                  surfaceTensor (zipWith (joinTypes env) leftShape rightShape) (joinTypes env leftElem rightElem)
              | otherwise ->
                  case (listView left', listView right') of
                    (Just leftList, Just rightList) -> joinTypes env leftList rightList
                    _ -> fallback
            _ ->
              case (listView left', listView right') of
                (Just leftList, Just rightList) -> joinTypes env leftList rightList
                _ -> fallback
  where
    left' = resolveType env left
    right' = resolveType env right
    fallback
      | left' == right' = left'
      | isSubtype env left' right' = right'
      | isSubtype env right' left' = left'
      | not (bothNumericLiterals left' right'),
        Just joinedNumeric <- joinNumericTypes left' right' =
          joinedNumeric
      | otherwise = flattenUnion (TUnion [left', right'])

-- | Decide whether two types can coexist under gradual typing rules.
--
-- Consistency is weaker than subtyping: `dynamic` is consistent with anything
-- even when it is not a subtype of that thing.
isConsistent :: AliasEnv -> Type -> Type -> Bool
isConsistent env left right =
  left' == TDynamic
    || right' == TDynamic
    || left' == right'
    || isSubtype env left' right'
    || isSubtype env right' left'
  where
    left' = resolveType env left
    right' = resolveType env right

-- | Structural subtyping relation used by the checker.
--
-- Functions are contravariant in their argument and covariant in their result;
-- records use width subtyping; literals subtype their primitive constructor.
isSubtype :: AliasEnv -> Type -> Type -> Bool
isSubtype env left right = go (resolveType env left) (resolveType env right)
  where
    go a b | a == b = True
    go (TLit (LString _)) ty | ty == tString = True
    go (TLit (LFloat _)) ty | ty == tFloat = True
    go (TLit lit) ty | ty == tNumber = isNumericLiteral lit
    go (TLit (LInt _)) ty | ty == tInt = True
    go (TLit (LInt n)) ty | ty == tNat = n >= 0
    go (TLit (LBool _)) ty | ty == tBool = True
    go ty other | ty == tNat, other == tInt = True
    go ty other | ty == tNat, other == tNumber = True
    go ty other | ty == tInt, other == tNumber = True
    go ty other | ty == tFloat, other == tNumber = True
    go (TTypeList xs) (TTypeList ys) = length xs == length ys && and (zipWith go xs ys)
    go _ TDynamic = True
    go TDynamic _ = False
    go (TUnion leftMembers) (TUnion rightMembers) =
      all (\member -> any (go member) rightMembers) leftMembers
    go a (TUnion members) = any (go a) members
    go (TUnion members) b = all (`go` b) members
    go a b
      | Just (leftLower, leftUpper, leftBase) <- rangeView a,
        Just (rightLower, rightUpper, rightBase) <- rangeView b =
          rangeBaseSubtype go leftLower leftUpper leftBase rightBase
            && rangeBoundsWithin leftLower leftUpper rightLower rightUpper
      | Just (leftLower, leftUpper, leftBase) <- rangeView a,
        b == tNat =
          (leftBase == tInt || leftBase == tNat) && nonNegativeIntegerBounds leftLower leftUpper
      | Just (_, _, leftBase) <- rangeView a =
          go leftBase b
    go a b
      | Just (rightLower, rightUpper, rightBase) <- rangeView b =
          go a rightBase && literalWithinRange a rightLower rightUpper
    go a b
      | Just (leftUnit, leftBase) <- unitView a,
        Just (rightUnit, rightBase) <- unitView b =
          leftUnit == rightUnit && go leftBase rightBase
      | Just (rightUnit, rightBase) <- unitView b =
          unitLabelLiteral rightUnit && numericLiteralType a && go a rightBase
    go a b
      | Just leftItems <- tupleView a,
        Just rightItems <- tupleView b =
          length leftItems == length rightItems && and (zipWith go leftItems rightItems)
      | Just leftList <- tupleListView a =
          go leftList b
    go a b
      | Just (leftShape, leftElem) <- tensorView a,
        Just (rightShape, rightElem) <- tensorView b =
          length leftShape == length rightShape
            && and (zipWith go leftShape rightShape)
            && (shapeDefinitelyEmpty leftShape || go leftElem rightElem)
      | Just leftList <- tensorListView a =
          go leftList b
    go (TFun leftMult a b) (TFun rightMult c d) =
      multiplicitySubtype leftMult rightMult && go c a && go b d
    go (TRecord fields) (TRecord expected) =
      all (\(name, ty) -> maybe False (`go` ty) (Map.lookup name fields)) (Map.toList expected)
    go (TApp f x) (TApp g y) = go f g && go x y
    go _ _ = False

multiplicitySubtype :: Multiplicity -> Multiplicity -> Bool
multiplicitySubtype left right =
  left == right
    || case (left, right) of
      (One, Many) -> True
      _ -> False

surfaceTensor :: [Type] -> Type -> Type
surfaceTensor dims elemTy =
  case dims of
    [lenTy] -> TApp (TApp (TCon "Vec") lenTy) elemTy
    [rowsTy, colsTy] -> TApp (TApp (TApp (TCon "Matrix") rowsTy) colsTy) elemTy
    _ -> TApp (TApp (TCon "Tensor") (TTypeList dims)) elemTy

listView :: Type -> Maybe Type
listView ty =
  case tupleListView ty of
    Just listTy -> Just listTy
    Nothing -> tensorListView ty

rangeView :: Type -> Maybe (Type, Type, Type)
rangeView ty =
  case collectApps ty of
    (TCon "Range", [lowerTy, upperTy, baseTy]) -> Just (lowerTy, upperTy, baseTy)
    _ -> Nothing

unitView :: Type -> Maybe (Type, Type)
unitView ty =
  case collectApps ty of
    (TCon "Unit", [unitTy, baseTy]) -> Just (unitTy, baseTy)
    _ -> Nothing

data NumericBound
  = NumericInt Integer
  | NumericFloat Double
  deriving (Eq, Ord, Show)

literalWithinRange :: Type -> Type -> Type -> Bool
literalWithinRange actual lowerTy upperTy =
  case (numericLiteralTypeValue actual, numericBoundValue lowerTy, numericBoundValue upperTy) of
    (Just actualValue, Just lowerValue, Just upperValue) ->
      compareNumericBound actualValue lowerValue /= LT && compareNumericBound actualValue upperValue /= GT
    _ -> False

rangeBoundsWithin :: Type -> Type -> Type -> Type -> Bool
rangeBoundsWithin leftLower leftUpper rightLower rightUpper =
  case (numericBoundValue leftLower, numericBoundValue leftUpper, numericBoundValue rightLower, numericBoundValue rightUpper) of
    (Just leftLow, Just leftHigh, Just rightLow, Just rightHigh) ->
      compareNumericBound leftLow rightLow /= LT && compareNumericBound leftHigh rightHigh /= GT
    _ -> False

nonNegativeIntegerBounds :: Type -> Type -> Bool
nonNegativeIntegerBounds lowerTy upperTy =
  case (numericBoundValue lowerTy, numericBoundValue upperTy) of
    (Just (NumericInt low), Just (NumericInt high)) -> low >= 0 && high >= 0 && low <= high
    _ -> False

numericLiteralType :: Type -> Bool
numericLiteralType = maybe False (const True) . numericLiteralTypeValue

numericLiteralTypeValue :: Type -> Maybe NumericBound
numericLiteralTypeValue = \case
  TLit (LInt n) -> Just (NumericInt n)
  TLit (LFloat n) -> Just (NumericFloat n)
  _ -> Nothing

numericBoundValue :: Type -> Maybe NumericBound
numericBoundValue = \case
  TLit (LInt n) -> Just (NumericInt n)
  TLit (LFloat n) -> Just (NumericFloat n)
  _ -> Nothing

compareNumericBound :: NumericBound -> NumericBound -> Ordering
compareNumericBound left right =
  compare (toDouble left) (toDouble right)
  where
    toDouble = \case
      NumericInt n -> fromInteger n
      NumericFloat n -> n

isNumericLiteral :: LiteralType -> Bool
isNumericLiteral = \case
  LInt _ -> True
  LFloat _ -> True
  _ -> False

numericBaseType :: Type -> Maybe Type
numericBaseType ty
  | ty == tNat = Just tNat
  | ty == tInt = Just tInt
  | ty == tFloat = Just tFloat
  | ty == tNumber = Just tNumber
numericBaseType (TLit (LInt _)) = Just tInt
numericBaseType (TLit (LFloat _)) = Just tFloat
numericBaseType ty
  | Just (_, _, baseTy) <- rangeView ty = numericBaseType baseTy
numericBaseType _ = Nothing

bothNumericLiterals :: Type -> Type -> Bool
bothNumericLiterals left right = numericLiteralType left && numericLiteralType right

joinNumericTypes :: Type -> Type -> Maybe Type
joinNumericTypes left right = do
  leftBase <- numericBaseType left
  rightBase <- numericBaseType right
  pure (joinNumericBases leftBase rightBase)

joinNumericBases :: Type -> Type -> Type
joinNumericBases left right
  | left == right = left
  | left == tNat, right == tInt = tInt
  | left == tInt, right == tNat = tInt
  | otherwise = tNumber

rangeBaseSubtype :: (Type -> Type -> Bool) -> Type -> Type -> Type -> Type -> Bool
rangeBaseSubtype subtype lowerTy upperTy leftBase rightBase
  | leftBase == rightBase = True
  | rightBase == tNat = (leftBase == tInt || leftBase == tNat) && nonNegativeIntegerBounds lowerTy upperTy
  | otherwise = subtype leftBase rightBase

unitLabelLiteral :: Type -> Bool
unitLabelLiteral = \case
  TLit (LString _) -> True
  _ -> False

shapeDefinitelyEmpty :: [Type] -> Bool
shapeDefinitelyEmpty = any typeDefinitelyZero

typeDefinitelyZero :: Type -> Bool
typeDefinitelyZero = \case
  TLit (LInt 0) -> True
  TUnion members -> not (null members) && all typeDefinitelyZero members
  ty
    | Just (lowerTy, upperTy, _) <- rangeView ty ->
        case (numericBoundValue lowerTy, numericBoundValue upperTy) of
          (Just (NumericInt 0), Just (NumericInt 0)) -> True
          _ -> False
  _ -> False
