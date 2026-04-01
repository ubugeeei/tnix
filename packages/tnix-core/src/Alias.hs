-- | Alias expansion and type-pattern utilities.
--
-- This module is the workhorse behind generic aliases, higher-kinded encodings,
-- and TypeScript-style `infer` patterns. The rest of the checker treats these
-- capabilities as structural rewrites over 'Type'.
module Alias
  ( AliasEnv,
    collectApps,
    expandAliases,
    flattenUnion,
    matchPattern,
    mkAliasEnv,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Type

-- | Environment keyed by alias name.
type AliasEnv = Map Name TypeAlias

-- | Build an alias environment from parsed declarations.
--
-- Later declarations with the same name win, which mirrors the simple \"last
-- definition in scope\" strategy used elsewhere in this prototype.
mkAliasEnv :: [TypeAlias] -> AliasEnv
mkAliasEnv = Map.fromList . map (\alias -> (typeAliasName alias, alias))

-- | Normalize nested unions and remove duplicates.
--
-- This keeps joins and alias expansion from producing deeply nested `a | (b |
-- c)` shapes that are annoying for both users and subsequent algorithms.
flattenUnion :: Type -> Type
flattenUnion (TUnion members) =
  case foldr insert [] (concatMap expand members) of
    [] -> TUnion []
    [single] -> single
    xs -> TUnion xs
  where
    expand (TUnion xs) = concatMap expand xs
    expand other = [other]
    insert item acc | item `elem` acc = acc
    insert item acc = item : acc
flattenUnion other = other

-- | Split a left-associated type application into its head and arguments.
--
-- `F A B` is represented as `TApp (TApp F A) B`, while many alias algorithms
-- want the pair `(F, [A, B])`.
collectApps :: Type -> (Type, [Type])
collectApps = go []
  where
    go acc (TApp f x) = go (x : acc) f
    go acc headTy = (headTy, acc)

-- | Expand type aliases up to a fixed recursion budget.
--
-- The implementation is intentionally eager enough to make hovers and emitted
-- declarations useful, while still guarding against runaway self-recursive
-- aliases.
expandAliases :: AliasEnv -> Type -> Type
expandAliases env = go 0
  where
    go :: Int -> Type -> Type
    go depth ty
      | depth > 32 = ty
      | otherwise =
        case ty of
          TCon name
            | Just alias <- Map.lookup name env,
              null (typeAliasParams alias) ->
                go (depth + 1) (typeAliasBody alias)
          TTypeList items -> TTypeList (map (go (depth + 1)) items)
          TFun a b -> TFun (go (depth + 1) a) (go (depth + 1) b)
          TRecord fields -> TRecord (fmap (go (depth + 1)) fields)
          TUnion members -> flattenUnion (TUnion (map (go (depth + 1)) members))
          TApp f x -> reduce depth (go (depth + 1) f) (go (depth + 1) x)
          TForall vars body -> TForall vars (go (depth + 1) body)
          TConditional a b c d -> TConditional (go (depth + 1) a) (go (depth + 1) b) (go (depth + 1) c) (go (depth + 1) d)
          other -> other

    reduce :: Int -> Type -> Type -> Type
    reduce depth f x =
      case collectApps (TApp f x) of
        (TCon name, args)
          | Just alias <- Map.lookup name env,
            length args >= length (typeAliasParams alias) ->
              let (used, rest) = splitAt (length (typeAliasParams alias)) args
                  subst = Map.fromList (zip (typeAliasParams alias) used)
                  body = substituteTypeVars subst (typeAliasBody alias)
               in foldl TApp (go (depth + 1) body) rest
        (headTy, args) -> foldl TApp headTy args

-- | Match a concrete type against a pattern containing 'TInfer' placeholders.
--
-- This is the structural heart of conditional types: when a pattern matches, we
-- recover the inferred bindings and substitute them into the `true` branch.
matchPattern :: Type -> Type -> Maybe (Map Name Type)
matchPattern actual patternTy = go Map.empty actual patternTy
  where
    go env value (TInfer name) =
      case Map.lookup name env of
        Nothing -> Just (Map.insert name value env)
        Just prev | prev == value -> Just env
        _ -> Nothing
    go env (TFun a b) (TFun c d) = go env a c >>= \env' -> go env' b d
    go env (TRecord a) (TRecord b) = foldl step (Just env) (Map.toList b)
      where
        step acc (name, ty) = do
          env' <- acc
          actualTy <- Map.lookup name a
          go env' actualTy ty
    go env (TTypeList actualItems) (TTypeList patternItems)
      | length actualItems == length patternItems =
          foldl
            (\acc (actualTy, nextPatternTy) -> acc >>= \env' -> go env' actualTy nextPatternTy)
            (Just env)
            (zip actualItems patternItems)
      | otherwise = Nothing
    go env (TApp a b) (TApp c d) = go env a c >>= \env' -> go env' b d
    go env value other | value == other = Just env
    go _ _ _ = Nothing
