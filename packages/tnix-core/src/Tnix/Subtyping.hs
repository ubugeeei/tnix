{-# LANGUAGE OverloadedStrings #-}

module Tnix.Subtyping
  ( isConsistent,
    isSubtype,
    joinTypes,
    lookupRecordField,
    resolveType,
  )
where

import Control.Applicative ((<|>))
import Data.Map.Strict qualified as Map
import Tnix.Alias
import Tnix.Type

resolveType :: AliasEnv -> Type -> Type
resolveType env = go 0 . expandAliases env . eraseForall
  where
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

lookupRecordField :: AliasEnv -> Type -> Name -> Maybe Type
lookupRecordField env ty field =
  case resolveType env ty of
    TRecord fields -> Map.lookup field fields
    TUnion members -> foldr (\member acc -> acc <|> lookupRecordField env member field) Nothing members
    _ -> Nothing

joinTypes :: AliasEnv -> Type -> Type -> Type
joinTypes env left right
  | left' == right' = left'
  | isSubtype env left' right' = right'
  | isSubtype env right' left' = left'
  | otherwise = flattenUnion (TUnion [left', right'])
  where
    left' = resolveType env left
    right' = resolveType env right

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
