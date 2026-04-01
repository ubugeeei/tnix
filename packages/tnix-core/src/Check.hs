{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Gradual type checker and local inference engine for tnix.
--
-- The checker intentionally aims for useful incremental feedback rather than
-- whole-program soundness. `dynamic` is built in, imports can be typed from
-- ambient declarations, and remaining inference variables are surfaced as
-- polymorphic schemes instead of forcing runtime evidence.
module Check
  ( CheckContext (..),
    CheckResult (..),
    checkProgram,
  )
where

import Control.Monad (foldM, forM, when, zipWithM)
import Control.Monad.State.Strict
import Data.List (group, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.FilePath ((</>), isAbsolute, joinPath, normalise, splitDirectories, takeDirectory)
import Alias
import Indexed
import Subtyping
import Syntax
import Type

-- | Inputs required to analyze one file.
--
-- `checkAmbient` represents the ambient world visible from the current file,
-- while `checkAliases` contains both local aliases and any imported declaration
-- aliases that were discovered by the driver.
data CheckContext = CheckContext
  { checkAliases :: AliasEnv,
    checkAmbient :: Map FilePath Scheme,
    checkFile :: FilePath
  }

-- | User-visible results produced by checking a program.
--
-- The root scheme describes the file's resulting expression, while
-- `resultBindings` records the final types of `let`-bound names for CLI output
-- and LSP hover.
data CheckResult = CheckResult
  { resultRoot :: Maybe Scheme,
    resultBindings :: Map Name Scheme
  }
  deriving (Eq, Show)

data InferState = InferState {nextMeta :: Int, substitutions :: Map Int Type}
type InferM = StateT InferState (Either String)
type TypeEnv = Map Name Scheme

-- | Check a parsed program and infer its public types.
--
-- The built-in environment is intentionally tiny: `builtins` is left fully
-- dynamic and `import` only promises that a path yields something. Ambient
-- declarations refine imports when they are available.
checkProgram :: CheckContext -> Program -> Either String CheckResult
checkProgram ctx program =
  evalStateT (inferTop ctx builtins) (InferState 0 Map.empty)
  where
    builtins = Map.fromList [("builtins", Scheme [] tDynamic), ("import", Scheme [] (TFun tPath tDynamic))]

    inferTop local env = case programExpr program of
      Nothing -> pure (CheckResult Nothing Map.empty)
      Just (ELet items body) -> do
        (env', bindings) <- inferLet local env items
        ty <- inferExpr local env' body >>= zonk
        pure (CheckResult (Just (closeMetas ty)) bindings)
      Just expr -> do
        ty <- inferExpr local env expr >>= zonk
        pure (CheckResult (Just (closeMetas ty)) Map.empty)

inferExpr :: CheckContext -> TypeEnv -> Expr -> InferM Type
inferExpr ctx env = \case
  EVar name -> maybe (lift (Left ("unbound name: " <> show name))) instantiate (Map.lookup name env)
  EString text -> pure (TLit (LString text))
  EInt n -> pure (TLit (LInt n))
  EBool b -> pure (TLit (LBool b))
  ENull -> pure tNull
  EPath _ -> pure tPath
  ELambda (PVar name ann) body -> do
    argTy <- maybe freshMeta pure ann
    bodyTy <- inferExpr ctx (Map.insert name (Scheme [] argTy) env) body
    pure (TFun argTy bodyTy)
  EApp (EVar "import") (EPath raw) ->
    maybe (pure tDynamic) instantiate (Map.lookup (resolvePath (checkFile ctx) raw) (checkAmbient ctx))
  EApp fun arg -> do
    funTy <- inferExpr ctx env fun >>= zonk
    argTy <- inferExpr ctx env arg
    if resolveType (checkAliases ctx) funTy == tDynamic
      then pure tDynamic
      else do
        outTy <- freshMeta
        _ <- unify ctx funTy (TFun argTy outTy)
        zonk outTy
  ELet items body -> do
    (env', _) <- inferLet ctx env items
    inferExpr ctx env' body
  EAttrSet items -> do
    fields <- concat <$> traverse inferAttr items
    case duplicateNames (map fst fields) of
      dup : _ -> lift (Left ("duplicate attribute: " <> show dup))
      [] -> pure (TRecord (Map.fromList fields))
    where
      inferAttr = \case
        AttrField name expr -> do
          ty <- inferExpr ctx env expr
          pure [(name, ty)]
        AttrInherit names ->
          traverse (\name -> inferExpr ctx env (EVar name) >>= \ty -> pure (name, ty)) names
  ESelect base fields -> do
    baseTy <- inferExpr ctx env base
    foldM step baseTy fields
    where
      step ty field =
        case lookupRecordField (checkAliases ctx) ty field of
          Just fieldTy -> instantiate (schemeFromAnnotation fieldTy)
          Nothing -> lift (Left ("missing field " <> show field))
  EIf cond yesExpr noExpr -> do
    _ <- inferExpr ctx env cond >>= unify ctx tBool
    yesTy <- inferExpr ctx env yesExpr
    noTy <- inferExpr ctx env noExpr
    pure (joinTypes (checkAliases ctx) yesTy noTy)
  EList members ->
    traverse (inferExpr ctx env) members
      >>= pure . inferListType (joinTypes (checkAliases ctx))

inferLet :: CheckContext -> TypeEnv -> [LetItem] -> InferM (TypeEnv, Map Name Scheme)
inferLet ctx env items = do
  let sigs = Map.fromList [(name, schemeFromAnnotation ty) | LetSignature name ty <- items]
      binds = [(name, expr) | LetBinding name expr <- items]
      bindNames = map fst binds
      missing = filter (`notElem` bindNames) (Map.keys sigs)
      duplicateSigs = duplicateNames [name | LetSignature name _ <- items]
      duplicateBinds = duplicateNames bindNames
      placeholder name = do
        scheme <- maybe (Scheme [] <$> freshMeta) pure (Map.lookup name sigs)
        pure (name, scheme)
  when (not (null duplicateSigs)) (lift (Left ("duplicate signatures: " <> show duplicateSigs)))
  when (not (null duplicateBinds)) (lift (Left ("duplicate bindings: " <> show duplicateBinds)))
  when (not (null missing)) (lift (Left ("missing bindings for signatures: " <> show missing)))
  placeholders <- Map.fromList <$> traverse placeholder bindNames
  let recursiveEnv = placeholders <> env
  inferred <- forM binds $ \(name, expr) -> do
    expected <- instantiate (recursiveEnv Map.! name)
    actual <- inferExpr ctx recursiveEnv expr
    _ <- unify ctx actual expected
    resolved <- zonk expected
    pure (name, maybe (closeMetas resolved) id (Map.lookup name sigs))
  let finals = Map.fromList inferred
  pure (finals <> env, finals)

instantiate :: Scheme -> InferM Type
instantiate (Scheme vars ty) = do
  reps <- traverse (const freshMeta) vars
  pure (substituteTypeVars (Map.fromList (zip vars reps)) ty)

freshMeta :: InferM Type
freshMeta = do
  st <- get
  put st {nextMeta = nextMeta st + 1}
  pure (TMeta (nextMeta st))

zonk :: Type -> InferM Type
zonk ty = substituteMetas <$> gets substitutions <*> pure ty

unify :: CheckContext -> Type -> Type -> InferM Type
unify ctx left right = do
  left' <- normalizeIndexedType <$> zonk left
  right' <- normalizeIndexedType <$> zonk right
  case (left', right') of
    (TMeta n, TMeta m) | n == m -> pure left'
    (TMeta n, ty) -> bindMeta n ty
    (ty, TMeta n) -> bindMeta n ty
    (TTypeList xs, TTypeList ys)
      | length xs == length ys ->
          TTypeList <$> zipWithM (unify ctx) xs ys
    (TFun a b, TFun c d) -> TFun <$> unify ctx a c <*> unify ctx b d
    (TRecord a, TRecord b) -> unifyRecord a b
    (TApp f x, TApp g y) -> TApp <$> unify ctx f g <*> unify ctx x y
    _ | left' == right' -> pure left'
    _ | isSubtype (checkAliases ctx) left' right' -> pure right'
    _ | isSubtype (checkAliases ctx) right' left' -> pure left'
    _ | isConsistent (checkAliases ctx) left' right' -> pure (joinTypes (checkAliases ctx) left' right')
    _ -> lift (Left ("type mismatch: " <> show left' <> " vs " <> show right'))
  where
    unifyRecord a b
      | Map.keysSet b `Set.isSubsetOf` Map.keysSet a =
          TRecord <$> traverseWithKey (\name ty -> unify ctx ty (b Map.! name)) b
      | Map.keysSet a `Set.isSubsetOf` Map.keysSet b =
          TRecord <$> traverseWithKey (\name ty -> unify ctx ty (a Map.! name)) a
      | otherwise = lift (Left ("record mismatch: " <> show a <> " vs " <> show b))
    traverseWithKey f = fmap Map.fromList . traverse (\(k, v) -> f k v >>= \ty -> pure (k, ty)) . Map.toList

bindMeta :: Int -> Type -> InferM Type
bindMeta n ty = do
  resolved <- zonk ty
  when (resolved == TMeta n || n `Set.member` freeMetas resolved) (lift (Left "occurs check failed"))
  modify' (\st -> st {substitutions = Map.insert n resolved (substitutions st)})
  pure resolved

resolvePath :: FilePath -> FilePath -> FilePath
resolvePath from target
  | isAbsolute target = collapseParentSegments target
  | otherwise = collapseParentSegments (takeDirectory from </> target)

collapseParentSegments :: FilePath -> FilePath
collapseParentSegments = joinPath . foldl step [] . splitDirectories . normalise
  where
    step acc "." = acc
    step [root] ".." | isAbsoluteRoot root = [root]
    step [] ".." = [".."]
    step acc ".." =
      case reverse acc of
        [] -> [".."]
        root : rest
          | isAbsoluteRoot root -> reverse (root : rest)
        _ : rest -> reverse rest
    step acc part = acc <> [part]
    isAbsoluteRoot part = part == "/"

duplicateNames :: Ord a => [a] -> [a]
duplicateNames = foldr step [] . group . sort
  where
    step xs acc =
      case xs of
        first : _ | length xs > 1 -> first : acc
        _ -> acc
