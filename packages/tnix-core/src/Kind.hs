{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Kind inference and validation for tnix types.
--
-- The core type representation already keeps higher-kinded applications
-- first-class via 'TApp'. This module turns that raw expressiveness into an
-- actual feature by inferring the kinds of aliases and rejecting impossible
-- applications such as `Int String` or `Twice Int`.
module Kind
  ( AliasKindEnv,
    inferAliasKinds,
    inferTypeKind,
    validateProgramKinds,
  )
where

import Control.Monad (foldM, replicateM, void)
import Control.Monad.State.Strict (StateT, evalStateT, get, lift, modify', put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Alias (mkAliasEnv)
import Syntax
import Type

-- | Kind environment keyed by type constructor name.
type AliasKindEnv = Map Name Kind

data AliasPlaceholder = AliasPlaceholder
  { placeholderParams :: [Kind],
    placeholderResult :: Kind
  }

data KindState = KindState
  { nextKindMeta :: Int,
    kindSubstitutions :: Map Int Kind,
    flexibleKinds :: Map Name Kind
  }

type KindM = StateT KindState (Either String)

-- | Infer kinds for the currently-visible alias environment.
--
-- Alias kinds are inferred structurally from their bodies, so higher-kinded
-- parameters do not need explicit annotations. Unknown external constructors
-- stay flexible, which keeps ambient declaration authoring incremental.
inferAliasKinds :: [TypeAlias] -> Either String AliasKindEnv
inferAliasKinds aliases =
  evalStateT (inferAll (Map.elems (mkAliasEnv aliases))) initialState
  where
    initialState = KindState {nextKindMeta = 0, kindSubstitutions = Map.empty, flexibleKinds = Map.empty}

-- | Infer the kind of one type under a known alias environment.
--
-- Callers usually pair this with `validateProgramKinds`, but the unit tests use
-- it directly to lock higher-kinded reductions and failure cases in place.
inferTypeKind :: AliasKindEnv -> Type -> Either String Kind
inferTypeKind aliasKinds ty =
  evalStateT (inferKind env mempty ty >>= zonkKind) initialState
  where
    env = builtinKinds <> aliasKinds
    initialState =
      KindState
        { nextKindMeta = nextKindSeed aliasKinds,
          kindSubstitutions = Map.empty,
          flexibleKinds = Map.empty
        }

-- | Validate all type-bearing surfaces in a parsed program.
--
-- Alias bodies may themselves be higher-kinded, so only annotations that sit in
-- term-facing positions are required to have kind `Type`.
validateProgramKinds :: [TypeAlias] -> Program -> Either String AliasKindEnv
validateProgramKinds aliases program = do
  aliasKinds <- inferAliasKinds aliases
  let env = builtinKinds <> aliasKinds
  let initialState =
        KindState
          { nextKindMeta = nextKindSeed aliasKinds,
            kindSubstitutions = Map.empty,
            flexibleKinds = Map.empty
          }
  void $
    evalStateT
      (do
          traverse_ (validateKind env "ambient entry" . ambientEntryType) ambientDeclEntries
          traverse_ (validateKind env "term annotation") annotationTypes
      )
      initialState
  pure aliasKinds
  where
    ambientDeclEntries = concatMap ambientEntriesFromDecl (programAmbient program)
    annotationTypes = maybe [] (exprAnnotations . markedValue) (programExpr program)
    ambientEntriesFromDecl decl = ambientEntries decl

inferAll :: [TypeAlias] -> KindM AliasKindEnv
inferAll aliases = do
  placeholders <-
    fmap Map.fromList $
      traverse
        (\alias -> do
            params <- replicateM (length (typeAliasParams alias)) freshKindMeta
            result <- freshKindMeta
            pure (typeAliasName alias, AliasPlaceholder params result)
        )
        aliases
  let aliasKinds =
        Map.map
          (\placeholder -> foldr KFun (placeholderResult placeholder) (placeholderParams placeholder))
          placeholders
      env = builtinKinds <> aliasKinds
  traverse_ (inferAliasBody env placeholders) aliases
  traverse zonkKind aliasKinds

inferAliasBody :: AliasKindEnv -> Map Name AliasPlaceholder -> TypeAlias -> KindM ()
inferAliasBody env placeholders alias = do
  let placeholder = placeholders Map.! typeAliasName alias
      local = Map.fromList (zip (typeAliasParams alias) (placeholderParams placeholder))
  bodyKind <- inferKind env local (typeAliasBody alias)
  void (unifyKind bodyKind (placeholderResult placeholder))

inferKind :: AliasKindEnv -> Map Name Kind -> Type -> KindM Kind
inferKind env local = \case
  TVar name -> maybe (flexibleKind name) pure (Map.lookup name local)
  TCon name -> maybe (flexibleKind name) pure (Map.lookup name env)
  TMeta _ -> pure KType
  TLit _ -> pure KType
  TTypeList members -> traverse_ (inferTypeLeaf env local) members >> pure KType
  TAny -> pure KType
  TDynamic -> pure KType
  TUnknown -> pure KType
  TFun _ left right -> inferTypeLeaf env local left >> inferTypeLeaf env local right >> pure KType
  TRecord fields -> traverse_ (inferTypeLeaf env local) fields >> pure KType
  TUnion members -> traverse_ (inferTypeLeaf env local) members >> pure KType
  TApp fun arg -> do
    funKind <- inferKind env local fun
    argKind <- inferKind env local arg
    resultKind <- freshKindMeta
    _ <- unifyKind funKind (KFun argKind resultKind)
    zonkKind resultKind
  TForall vars body -> do
    locals <- traverse (const freshKindMeta) vars
    inferKind env (Map.union (Map.fromList (zip vars locals)) local) body
  TConditional actual patternTy yesTy noTy -> do
    actualKind <- inferKind env local actual
    patternKind <- inferKind env local patternTy
    _ <- unifyKind actualKind patternKind
    yesKind <- inferKind env local yesTy
    noKind <- inferKind env local noTy
    _ <- unifyKind yesKind noKind
    zonkKind yesKind
  TInfer name -> maybe (flexibleKind name) pure (Map.lookup name local)

inferTypeLeaf :: AliasKindEnv -> Map Name Kind -> Type -> KindM ()
inferTypeLeaf env local ty = do
  kind <- inferKind env local ty
  void (validateResolvedKind kind)

validateKind :: AliasKindEnv -> String -> Type -> KindM ()
validateKind env label ty = do
  kind <- inferKind env mempty ty
  finalKind <- validateResolvedKind kind
  case finalKind of
    KType -> pure ()
    other ->
      liftLeft
        ( label
            <> " must resolve to Type, but got "
            <> show other
            <> " for "
            <> show ty
        )

validateResolvedKind :: Kind -> KindM Kind
validateResolvedKind kind = do
  finalKind <- zonkKind kind
  case finalKind of
    KMeta n -> do
      modify' (\st -> st {kindSubstitutions = Map.insert n KType (kindSubstitutions st)})
      pure KType
    other -> pure other

freshKindMeta :: KindM Kind
freshKindMeta = do
  st <- get
  put st {nextKindMeta = nextKindMeta st + 1}
  pure (KMeta (nextKindMeta st))

flexibleKind :: Name -> KindM Kind
flexibleKind name = do
  st <- get
  case Map.lookup name (flexibleKinds st) of
    Just kind -> pure kind
    Nothing -> do
      kind <- freshKindMeta
      modify' (\state -> state {flexibleKinds = Map.insert name kind (flexibleKinds state)})
      pure kind

zonkKind :: Kind -> KindM Kind
zonkKind = \case
  KMeta n -> do
    subs <- kindSubstitutions <$> get
    case Map.lookup n subs of
      Nothing -> pure (KMeta n)
      Just kind -> zonkKind kind
  KFun left right -> KFun <$> zonkKind left <*> zonkKind right
  other -> pure other

unifyKind :: Kind -> Kind -> KindM Kind
unifyKind left right = do
  left' <- zonkKind left
  right' <- zonkKind right
  case (left', right') of
    (KMeta n, KMeta m) | n == m -> pure left'
    (KMeta n, kind) -> bindKindMeta n kind
    (kind, KMeta n) -> bindKindMeta n kind
    (KType, KType) -> pure KType
    (KFun a b, KFun c d) -> KFun <$> unifyKind a c <*> unifyKind b d
    _ -> liftLeft ("kind mismatch: " <> show left' <> " vs " <> show right')

bindKindMeta :: Int -> Kind -> KindM Kind
bindKindMeta n kind = do
  resolved <- zonkKind kind
  if resolved == KMeta n || occursKind n resolved
    then liftLeft "kind occurs check failed"
    else do
      modify' (\st -> st {kindSubstitutions = Map.insert n resolved (kindSubstitutions st)})
      pure resolved

occursKind :: Int -> Kind -> Bool
occursKind needle = \case
  KMeta n -> n == needle
  KFun left right -> occursKind needle left || occursKind needle right
  KType -> False

exprAnnotations :: Expr -> [Type]
exprAnnotations = \case
  EVar _ -> []
  EString _ -> []
  EFloat _ -> []
  EInt _ -> []
  EBool _ -> []
  ENull -> []
  EPath _ -> []
  ELambda (PVar _ annotation) body -> maybe [] pure annotation <> exprAnnotations body
  EApp fun arg -> exprAnnotations fun <> exprAnnotations arg
  ELet items body -> foldMap (letItemAnnotations . markedValue) items <> exprAnnotations body
  EAttrSet items -> foldMap attrAnnotations items
  ESelect base _ -> exprAnnotations base
  EIf cond yesExpr noExpr -> foldMap exprAnnotations [cond, yesExpr, noExpr]
  EList members -> foldMap exprAnnotations members
  ECast expr ty -> exprAnnotations expr <> [ty]

letItemAnnotations :: LetItem -> [Type]
letItemAnnotations = \case
  LetSignature _ ty -> [ty]
  LetBinding _ expr -> exprAnnotations expr

attrAnnotations :: AttrItem -> [Type]
attrAnnotations = \case
  AttrField _ expr -> exprAnnotations expr
  AttrInherit _ -> []

builtinKinds :: AliasKindEnv
builtinKinds =
  Map.fromList
    [ ("Bool", KType),
      ("Float", KType),
      ("Int", KType),
      ("List", KFun KType KType),
      ("Matrix", KFun KType (KFun KType (KFun KType KType))),
      ("Nat", KType),
      ("Null", KType),
      ("Number", KType),
      ("Path", KType),
      ("String", KType),
      ("Range", KFun KType (KFun KType (KFun KType KType))),
      ("Tensor", KFun KType (KFun KType KType)),
      ("Tuple", KFun KType KType),
      ("Unit", KFun KType (KFun KType KType)),
      ("Vec", KFun KType (KFun KType KType))
    ]

nextKindSeed :: AliasKindEnv -> Int
nextKindSeed env = maybe 0 (+ 1) (maximumMaybe (foldMap kindMetas env))

kindMetas :: Kind -> [Int]
kindMetas = \case
  KType -> []
  KMeta n -> [n]
  KFun left right -> kindMetas left <> kindMetas right

maximumMaybe :: [Int] -> Maybe Int
maximumMaybe [] = Nothing
maximumMaybe values = Just (maximum values)

traverse_ :: Foldable f => (a -> KindM b) -> f a -> KindM ()
traverse_ step = foldM (\() item -> step item >> pure ()) ()

liftLeft :: String -> KindM a
liftLeft = lift . Left
