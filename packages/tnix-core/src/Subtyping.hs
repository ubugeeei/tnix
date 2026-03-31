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
import Type

-- | Reduce aliases, erase top-level `forall`, and evaluate conditional types.
--
-- Most higher-level algorithms call this before comparing types so they see a
-- normalized structural view instead of the user-written surface form.
resolveType :: AliasEnv -> Type -> Type
resolveType env = go 0 . expandAliases env . eraseForall
  where
    go :: Int -> Type -> Type
    go depth ty
      | depth > 32 = ty
      | otherwise =
          case expandAliases env ty of
            TFun a b -> TFun (go (depth + 1) a) (go (depth + 1) b)
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
joinTypes env left right
  | left' == right' = left'
  | isSubtype env left' right' = right'
  | isSubtype env right' left' = left'
  | otherwise = flattenUnion (TUnion [left', right'])
  where
    left' = resolveType env left
    right' = resolveType env right

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
    go (TLit (LInt _)) ty | ty == tInt = True
    go (TLit (LBool _)) ty | ty == tBool = True
    go _ TDynamic = True
    go TDynamic _ = False
    go a (TUnion members) = any (go a) members
    go (TUnion members) b = all (`go` b) members
    go (TFun a b) (TFun c d) = go c a && go b d
    go (TRecord fields) (TRecord expected) =
      all (\(name, ty) -> maybe False (`go` ty) (Map.lookup name fields)) (Map.toList expected)
    go (TApp f x) (TApp g y) = go f g && go x y
    go _ _ = False
