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

import Control.Applicative ((<|>))
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
--
-- The checker follows a "best effort but explicit" policy:
--
-- * annotations are respected when they are structurally satisfied,
-- * gradual behavior only kicks in when `dynamic` is genuinely involved,
-- * unresolved inference variables are closed into stable schemes before
--   results escape the module.
--
-- Representative examples:
--
-- @
-- let id = x: x; in id
--   => forall a. a -> a
--
-- let xs :: Vec (Range 2 4 Nat) Int; xs = [1 2 3]; in xs
--   => accepted
--
-- let xs :: Vec (2 | Range 4 8 Nat) Int; xs = [1 2 3]; in xs
--   => rejected
-- @
checkProgram :: CheckContext -> Program -> Either String CheckResult
checkProgram ctx program =
  evalStateT (inferTop ctx builtins) (InferState 0 Map.empty)
  where
    builtinsScheme = Map.findWithDefault (Scheme [] tDynamic) "builtins" (checkAmbient ctx)
    builtins = Map.fromList [("builtins", builtinsScheme), ("import", Scheme [] (TFun Many tPath tDynamic))]

    inferTop local env = case programExpr program of
      Nothing -> pure (CheckResult Nothing Map.empty)
      Just markedExpr -> inferRootExpression local env markedExpr

-- | Infer the type of one expression under the current local environment.
--
-- The function is syntax-directed except for applications and annotations,
-- where it consults `constrain`/`unify` to reconcile inferred and expected
-- structure. List literals delegate to `inferListType`, which means exact
-- vector, matrix, or tensor shapes can be recovered directly from surface list
-- syntax.
--
-- Representative examples:
--
-- @
-- inferExpr [] [1 2]
--   => Vec 2 (1 | 2)
--
-- inferExpr [] [[1 2] [3 4]]
--   => Matrix 2 2 (1 | 2 | 3 | 4)
--
-- inferExpr [] (import ./unknown.nix)
--   => dynamic
--
-- inferExpr [] ({ value = 1; } as { value :: Int; })
--   => { value :: Int; }
-- @
inferExpr :: CheckContext -> TypeEnv -> Expr -> InferM Type
inferExpr ctx env = \case
  EVar name -> maybe (lift (Left ("unbound name: " <> show name))) instantiate (Map.lookup name env)
  EString text -> pure (TLit (LString text))
  EFloat n -> pure (TLit (LFloat n))
  EInt n -> pure (TLit (LInt n))
  EBool b -> pure (TLit (LBool b))
  ENull -> pure tNull
  EPath _ -> pure tPath
  ELambda (PVar name ann) body -> do
    argTy <- maybe freshMeta pure ann
    bodyTy <- inferExpr ctx (Map.insert name (Scheme [] argTy) env) body
    pure (TFun (inferLambdaMultiplicity name body) argTy bodyTy)
  EApp (EVar "import") (EPath raw) ->
    maybe (pure tDynamic) instantiate (Map.lookup (resolvePath (checkFile ctx) raw) (checkAmbient ctx))
  EApp fun arg -> do
    funTy <- inferExpr ctx env fun >>= zonk
    argTy <- inferExpr ctx env arg
    let resolvedFunTy = resolveType (checkAliases ctx) funTy
    if resolvedFunTy == tDynamic
      then pure tDynamic
      else
        if resolvedFunTy == tAny
          then pure tAny
          else
            case resolvedFunTy of
              TFun _ domTy outTy -> constrain ctx argTy domTy *> zonk outTy
              _ -> do
                outTy <- freshMeta
                _ <- unify ctx funTy (TFun Many argTy outTy)
                zonk outTy
  EAdd left right -> inferAddition ctx env left right
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
        zonk ty >>= \resolvedTy ->
          if resolveType (checkAliases ctx) resolvedTy == tAny
            then pure tAny
            else
              case lookupRecordField (checkAliases ctx) resolvedTy field of
                Just fieldTy -> instantiate (schemeFromAnnotation fieldTy)
                Nothing -> lift (Left ("missing field " <> show field))
  EIf cond yesExpr noExpr -> do
    condTy <- inferExpr ctx env cond
    _ <- constrain ctx condTy tBool
    yesTy <- inferExpr ctx env yesExpr
    noTy <- inferExpr ctx env noExpr
    pure (joinTypes (checkAliases ctx) yesTy noTy)
  EList members ->
    traverse (inferExpr ctx env) members
      >>= pure . inferListType (joinTypes (checkAliases ctx))
  ECast expr assertedTy -> do
    actualTy <- inferExpr ctx env expr
    checkCast ctx actualTy assertedTy

-- | Infer a mutually recursive `let` group.
--
-- The algorithm proceeds in three phases:
--
-- * collect user signatures,
-- * allocate placeholders so recursive bindings may refer to one another,
-- * infer each body and constrain it against its placeholder/signature.
--
-- This arrangement keeps explicit signatures authoritative while still allowing
-- recursive inference for unannotated bindings.
--
-- Representative examples:
--
-- @
-- let
--   id :: forall a. a -> a;
--   id = x: x;
-- in id
--   => keeps the declared polymorphic scheme for `id`
--
-- let
--   value = value;
-- in value
--   => allocates a placeholder first, then constrains recursively
-- @
inferLet :: CheckContext -> TypeEnv -> [Marked LetItem] -> InferM (TypeEnv, Map Name Scheme)
inferLet ctx env items = do
  let sigs = Map.fromList [(name, schemeFromAnnotation ty) | Marked _ (LetSignature name ty) <- items]
      sigDirectives = Map.fromList [(name, directive) | Marked (Just directive) (LetSignature name _) <- items]
      binds = [(name, expr, directive) | Marked directive (LetBinding name expr) <- items]
      bindNames = [name | (name, _, _) <- binds]
      missing = filter (`notElem` bindNames) (Map.keys sigs)
      duplicateSigs = duplicateNames [name | Marked _ (LetSignature name _) <- items]
      duplicateBinds = duplicateNames bindNames
      placeholder name = do
        scheme <- maybe (Scheme [] <$> freshMeta) pure (Map.lookup name sigs)
        pure (name, scheme)
  when (not (null duplicateSigs)) (lift (Left ("duplicate signatures: " <> show duplicateSigs)))
  when (not (null duplicateBinds)) (lift (Left ("duplicate bindings: " <> show duplicateBinds)))
  when (not (null missing)) (lift (Left ("missing bindings for signatures: " <> show missing)))
  placeholders <- Map.fromList <$> traverse placeholder bindNames
  let recursiveEnv = placeholders <> env
  inferred <- forM binds $ \(name, expr, inlineDirective) -> do
    expected <- instantiate (recursiveEnv Map.! name)
    let directive = inlineDirective <|> Map.lookup name sigDirectives
    attempt <-
      catchInfer $ do
        actual <- inferExpr ctx recursiveEnv expr
        _ <- constrain ctx actual expected
        zonk expected
    resolved <-
      case (directive, attempt) of
        (Nothing, Right ty) -> pure ty
        (Nothing, Left err) -> lift (Left err)
        (Just TnixIgnore, Right ty) -> pure ty
        (Just TnixIgnore, Left _) -> recoverSuppressedType ctx expected
        (Just TnixExpected, Left _) -> recoverSuppressedType ctx expected
        (Just TnixExpected, Right _) ->
          lift (Left ("unused @tnix-expected directive on binding " <> show name))
    pure (name, maybe (closeMetas resolved) id (Map.lookup name sigs))
  let finals = Map.fromList inferred
  pure (finals <> env, finals)

-- | Instantiate a polymorphic scheme by replacing quantified variables with
-- fresh inference metas.
instantiate :: Scheme -> InferM Type
instantiate (Scheme vars ty) = do
  reps <- traverse (const freshMeta) vars
  pure (substituteTypeVars (Map.fromList (zip vars reps)) ty)

-- | Allocate a fresh inference meta variable.
freshMeta :: InferM Type
freshMeta = do
  st <- get
  put st {nextMeta = nextMeta st + 1}
  pure (TMeta (nextMeta st))

-- | Apply the current substitution set to a type.
--
-- This is the main "read your work back" operation of the inference engine:
-- callers use it after unification or binding a meta to recover the most
-- up-to-date structural view.
zonk :: Type -> InferM Type
zonk ty = substituteMetas <$> gets substitutions <*> pure ty

inferRootExpression :: CheckContext -> TypeEnv -> Marked Expr -> InferM CheckResult
inferRootExpression ctx env (Marked directive expr) = do
  attempt <-
    catchInfer $
      case expr of
        ELet items body -> do
          (env', bindings) <- inferLet ctx env items
          ty <- inferExpr ctx env' body >>= zonk
          pure (CheckResult (Just (closeMetas ty)) bindings)
        _ -> do
          ty <- inferExpr ctx env expr >>= zonk
          pure (CheckResult (Just (closeMetas ty)) Map.empty)
  case (directive, attempt) of
    (Nothing, Right result) -> pure result
    (Nothing, Left err) -> lift (Left err)
    (Just TnixIgnore, Right result) -> pure result
    (Just TnixIgnore, Left _) -> pure (CheckResult (Just (Scheme [] tDynamic)) Map.empty)
    (Just TnixExpected, Left _) -> pure (CheckResult (Just (Scheme [] tDynamic)) Map.empty)
    (Just TnixExpected, Right _) -> lift (Left "unused @tnix-expected directive on root expression")

catchInfer :: InferM a -> InferM (Either String a)
catchInfer action = do
  snapshot <- get
  case runStateT action snapshot of
    Left err -> pure (Left err)
    Right (value, state') -> put state' >> pure (Right value)

recoverSuppressedType :: CheckContext -> Type -> InferM Type
recoverSuppressedType ctx expected = constrain ctx tDynamic expected *> zonk expected

-- | Check that an inferred type satisfies an expected type.
--
-- Compared with `unify`, `constrain` is intentionally directional. It is used
-- for user annotations and function arguments where one side represents an
-- obligation rather than an unknown peer.
--
-- A few policy choices are important here:
--
-- * exact subtyping succeeds immediately,
-- * gradual consistency is only accepted when `dynamic` participates,
-- * plain concrete mismatches do /not/ fall through to permissive unification,
-- * sequence types may compare through their structural `List` view when one
--   side explicitly asks for `List`.
--
-- Representative examples:
--
-- @
-- constrain (Vec 2 Int) (List Int)
--   => succeeds through the structural list view
--
-- constrain (Vec 3 Int) (Vec (2 | Range 4 8 Nat) Int)
--   => fails
--
-- constrain dynamic String
--   => succeeds, because the mismatch is genuinely gradual
-- @
constrain :: CheckContext -> Type -> Type -> InferM Type
constrain ctx actual expected = do
  actual' <- normalizeIndexedType <$> zonk actual
  expected' <- normalizeIndexedType <$> zonk expected
  case (actual', expected') of
    _
      | Just actualList <- sequenceListView actual',
        isPlainListType expected' ->
          constrain ctx actualList expected'
      | isPlainListType actual',
        Just expectedList <- sequenceListView expected' ->
          constrain ctx actual' expectedList
    (TMeta n, TMeta m) | n == m -> pure actual'
    (TMeta n, ty) -> bindMeta n ty
    (ty, TMeta n) -> bindMeta n ty
    (TTypeList xs, TTypeList ys)
      | length xs == length ys ->
          TTypeList <$> zipWithM (constrain ctx) xs ys
    (TFun actualMult actualArg actualResult, TFun expectedMult expectedArg expectedResult)
      | multiplicitySubtype actualMult expectedMult ->
          TFun expectedMult <$> constrain ctx expectedArg actualArg <*> constrain ctx actualResult expectedResult
    _ | actual' == expected' -> pure expected'
    _ | isSubtype (checkAliases ctx) actual' expected' -> pure expected'
    _ | allowsGradualConsistency actual' expected' && isConsistent (checkAliases ctx) actual' expected' -> pure expected'
    _ | hasUnresolvedMetas actual' expected' -> unify ctx actual' expected'
    _ -> lift (Left ("type mismatch: " <> show actual' <> " vs " <> show expected'))

-- | Symmetric structural unification used while solving metas.
--
-- Unlike `constrain`, both sides are treated as peers here. When metas are
-- present the function may bind them, recursively unify structured types, or
-- join gradually consistent shapes when `dynamic` is involved.
--
-- Representative examples:
--
-- @
-- unify ?0 Int
--   => binds ?0 := Int
--
-- unify (List ?0) (List String)
--   => binds ?0 := String
--
-- unify Int String
--   => fails
-- @
unify :: CheckContext -> Type -> Type -> InferM Type
unify ctx left right = do
  left' <- normalizeIndexedType <$> zonk left
  right' <- normalizeIndexedType <$> zonk right
  case (left', right') of
    _
      | Just leftList <- sequenceListView left',
        isPlainListType right' ->
          unify ctx leftList right'
      | isPlainListType left',
        Just rightList <- sequenceListView right' ->
          unify ctx left' rightList
    (TMeta n, TMeta m) | n == m -> pure left'
    (TMeta n, ty) -> bindMeta n ty
    (ty, TMeta n) -> bindMeta n ty
    (TTypeList xs, TTypeList ys)
      | length xs == length ys ->
          TTypeList <$> zipWithM (unify ctx) xs ys
    (TFun leftMult a b, TFun rightMult c d)
      | leftMult == rightMult ->
          TFun leftMult <$> unify ctx a c <*> unify ctx b d
    (TRecord a, TRecord b) -> unifyRecord a b
    (TApp f x, TApp g y) -> TApp <$> unify ctx f g <*> unify ctx x y
    _ | left' == right' -> pure left'
    _ | isSubtype (checkAliases ctx) left' right' -> pure right'
    _ | isSubtype (checkAliases ctx) right' left' -> pure left'
    _ | allowsGradualConsistency left' right' && isConsistent (checkAliases ctx) left' right' -> pure (joinTypes (checkAliases ctx) left' right')
    _ -> lift (Left ("type mismatch: " <> show left' <> " vs " <> show right'))
  where
    unifyRecord a b
      | Map.keysSet b `Set.isSubsetOf` Map.keysSet a =
          TRecord <$> traverseWithKey (\name ty -> unify ctx ty (b Map.! name)) b
      | Map.keysSet a `Set.isSubsetOf` Map.keysSet b =
          TRecord <$> traverseWithKey (\name ty -> unify ctx ty (a Map.! name)) a
      | otherwise = lift (Left ("record mismatch: " <> show a <> " vs " <> show b))
    traverseWithKey f = fmap Map.fromList . traverse (\(k, v) -> f k v >>= \ty -> pure (k, ty)) . Map.toList

-- | Bind one inference meta to a solved type, performing the occurs check.
bindMeta :: Int -> Type -> InferM Type
bindMeta n ty = do
  resolved <- zonk ty
  when (resolved == TMeta n || n `Set.member` freeMetas resolved) (lift (Left "occurs check failed"))
  modify' (\st -> st {substitutions = Map.insert n resolved (substitutions st)})
  pure resolved

-- | Validate an explicit `expr as Type` assertion.
--
-- Casts deliberately live between plain assignment and fully-unsound escape
-- hatches. They are accepted when the two sides already overlap structurally,
-- when a gradual boundary such as `any`, `unknown`, or `dynamic` connects
-- them, or when the cast still contains unresolved inference metas that can be
-- solved by unification.
--
-- Representative examples:
--
-- @
-- { value = 1; } as { value :: Int; }
--   => accepted
--
-- import ./opaque.nix as { value :: String; }
--   => accepted when the import is `dynamic`
--
-- 1 as String
--   => rejected
-- @
checkCast :: CheckContext -> Type -> Type -> InferM Type
checkCast ctx actual expected = do
  actual' <- normalizeIndexedType <$> zonk actual
  expected' <- normalizeIndexedType <$> zonk expected
  let aliases = checkAliases ctx
  if hasUnresolvedMetas actual' expected'
    then unify ctx actual' expected' *> pure expected
    else
      if isSubtype aliases actual' expected'
        || isSubtype aliases expected' actual'
        || isConsistent aliases actual' expected'
        then pure expected
        else lift (Left ("invalid cast: " <> show actual' <> " as " <> show expected'))

inferAddition :: CheckContext -> TypeEnv -> Expr -> Expr -> InferM Type
inferAddition ctx env left right = do
  leftTy <- inferExpr ctx env left >>= zonk
  rightTy <- inferExpr ctx env right >>= zonk
  let aliases = checkAliases ctx
      leftResolved = resolveType aliases leftTy
      rightResolved = resolveType aliases rightTy
  if leftResolved == tAny || rightResolved == tAny
    then pure tAny
    else
      if leftResolved == tDynamic || rightResolved == tDynamic
        then pure tDynamic
        else do
          let expected = additionTarget leftResolved rightResolved
          _ <- constrain ctx leftTy expected
          _ <- constrain ctx rightTy expected
          zonk expected

additionTarget :: Type -> Type -> Type
additionTarget left right =
  case (numericFamily left, numericFamily right) of
    (Just leftBase, Just rightBase) -> joinNumericFamilies leftBase rightBase
    (Just knownBase, Nothing)
      | hasUnresolvedMetas left right -> widenSingleNumericFamily knownBase
    (Nothing, Just knownBase)
      | hasUnresolvedMetas left right -> widenSingleNumericFamily knownBase
    _ -> tNumber

numericFamily :: Type -> Maybe Type
numericFamily = \case
  ty
    | ty == tNat -> Just tNat
    | ty == tInt -> Just tInt
    | ty == tFloat -> Just tFloat
    | ty == tNumber -> Just tNumber
  TLit (LInt _) -> Just tInt
  TLit (LFloat _) -> Just tFloat
  _ -> Nothing

joinNumericFamilies :: Type -> Type -> Type
joinNumericFamilies left right
  | left == right = left
  | left == tNat, right == tInt = tInt
  | left == tInt, right == tNat = tInt
  | otherwise = tNumber

widenSingleNumericFamily :: Type -> Type
widenSingleNumericFamily ty
  | ty == tNat = tInt
  | ty == tInt = tInt
  | otherwise = tNumber

-- | Resolve an import path relative to the current source file.
--
-- Absolute paths are normalized but otherwise preserved. Relative paths are
-- interpreted against the directory containing the current file, mirroring how
-- Nix imports behave.
resolvePath :: FilePath -> FilePath -> FilePath
resolvePath from target
  | isAbsolute target = collapseParentSegments target
  | otherwise = collapseParentSegments (takeDirectory from </> target)

-- | Normalize `.` and `..` path segments without escaping an absolute root.
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

-- | Infer a lambda's multiplicity from its body usage count.
--
-- A binder used exactly once becomes linear; anything else becomes
-- unrestricted. This is intentionally local and syntactic.
inferLambdaMultiplicity :: Name -> Expr -> Multiplicity
inferLambdaMultiplicity name body
  | usageCount name body == 1 = One
  | otherwise = Many

-- | Count syntactic occurrences of a binder in an expression.
--
-- Shadowing stops the walk for the shadowed name, and recursive `let`
-- definitions are treated conservatively by not counting occurrences through a
-- re-bound name.
usageCount :: Name -> Expr -> Int
usageCount target = go
  where
    go = \case
      EVar name
        | name == target -> 1
        | otherwise -> 0
      EString _ -> 0
      EFloat _ -> 0
      EInt _ -> 0
      EBool _ -> 0
      ENull -> 0
      EPath _ -> 0
      ELambda (PVar name _) body
        | name == target -> 0
        | otherwise -> go body
      EApp fun arg -> go fun + go arg
      EAdd left right -> go left + go right
      ELet items body ->
        let names = [name | Marked _ (LetBinding name _) <- items]
         in if target `elem` names
              then 0
              else sum (map (letItemCount . markedValue) items) + go body
      EAttrSet items -> sum (map attrItemCount items)
      ESelect base _ -> go base
      EIf cond yesExpr noExpr -> go cond + go yesExpr + go noExpr
      EList items -> sum (map go items)
      ECast expr _ -> go expr
    letItemCount = \case
      LetSignature _ _ -> 0
      LetBinding _ expr -> go expr
    attrItemCount = \case
      AttrField _ expr -> go expr
      AttrInherit names -> length (filter (== target) names)

multiplicitySubtype :: Multiplicity -> Multiplicity -> Bool
multiplicitySubtype actual expected =
  actual == expected
    || case (actual, expected) of
      (One, Many) -> True
      _ -> False

-- | Widen a precise tuple/tensor into its structural list view when possible.
--
-- This is the bridge that lets exact sequence types interact with explicit
-- `List` annotations and ambient declarations.
sequenceListView :: Type -> Maybe Type
sequenceListView ty =
  case tensorListView ty of
    Just listTy -> Just listTy
    Nothing -> tupleListView ty

-- | Recognize the plain built-in `List a` shape.
isPlainListType :: Type -> Bool
isPlainListType ty =
  case collectApps ty of
    (TCon "List", [_]) -> True
    _ -> False

-- | Decide whether a consistency-based escape hatch is allowed.
--
-- Gradual consistency exists to smooth interop with genuinely dynamic values,
-- not to silently blur two unrelated concrete types.
allowsGradualConsistency :: Type -> Type -> Bool
allowsGradualConsistency left right = hasDynamic left || hasDynamic right

-- | Check whether either side still contains unsolved metas.
hasUnresolvedMetas :: Type -> Type -> Bool
hasUnresolvedMetas left right = not (Set.null (freeMetas left <> freeMetas right))

-- | Detect whether a type tree mentions `dynamic` anywhere inside it.
hasDynamic :: Type -> Bool
hasDynamic = \case
  TDynamic -> True
  TTypeList items -> any hasDynamic items
  TFun _ left right -> hasDynamic left || hasDynamic right
  TRecord fields -> any hasDynamic fields
  TUnion members -> any hasDynamic members
  TApp fun arg -> hasDynamic fun || hasDynamic arg
  TForall _ body -> hasDynamic body
  TConditional actual patternTy yesTy noTy ->
    any hasDynamic [actual, patternTy, yesTy, noTy]
  _ -> False
