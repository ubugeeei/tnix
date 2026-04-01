{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core type-language definitions used by every tnix phase.
--
-- The design deliberately models the \"type layer\" as data rather than as a
-- separate elaborated IR. That keeps the compiler simple: parsing, checking,
-- declaration emission, and LSP hover all inspect the same tree. It also fits
-- tnix's TypeScript-inspired strategy where types are erased before runtime and
-- may remain partially unresolved for a while.
module Type
  ( Kind (..),
    LiteralType (..),
    Multiplicity (..),
    Name,
    Scheme (..),
    Type (..),
    TypeAlias (..),
    closeMetas,
    eraseForall,
    freeMetas,
    freeMetasScheme,
    freeTypeVars,
    schemeFromAnnotation,
    substituteMetas,
    substituteTypeVars,
    tBool,
    tDynamic,
    tFloat,
    tInt,
    tList,
    tNat,
    tNull,
    tNumber,
    tPath,
    tString,
  )
where

import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

-- | Identifier type shared by terms and types.
--
-- tnix intentionally reuses plain textual names so that generated `.nix`
-- output stays close to source and ambient declarations can mirror existing
-- Nix code without a name-mangling phase.
type Name = Text

-- | Literal singleton types.
--
-- These provide the \"type puzzle\" building blocks that gradual systems often
-- rely on. Literal types participate in structural subtyping, joins, and
-- declaration emission, which lets the checker preserve precise information
-- until widening becomes necessary.
data LiteralType = LBool Bool | LFloat Double | LInt Integer | LString Text
  deriving (Eq, Ord, Show)

-- | The lightweight kind language used to validate higher-kinded types.
--
-- tnix does not surface kinds in source syntax yet, but it still infers them
-- so aliases such as `Compose f g a = f (g a)` are accepted while mistakes like
-- `Int String` are rejected early.
data Kind
  = KType
  | KFun Kind Kind
  | KMeta Int
  deriving (Eq, Ord, Show)

-- | Argument multiplicity for function arrows.
--
-- `One` models linear functions that must consume their argument exactly once,
-- while `Many` is the ordinary unrestricted arrow used by plain Nix code.
data Multiplicity
  = One
  | Many
  deriving (Eq, Ord, Show)

-- | The tnix type language.
--
-- A few design choices are worth calling out:
--
-- * 'TDynamic' is the gradual escape hatch and is never compiled to runtime
--   checks.
-- * 'TApp' keeps higher-kinded and alias applications first-class.
-- * 'TConditional' and 'TInfer' provide TypeScript-style type-level pattern
--   matching.
-- * 'TMeta' exists only during inference and is closed away before public
--   results are reported.
data Type
  = TVar Name
  | TCon Name
  | TMeta Int
  | TLit LiteralType
  | TTypeList [Type]
  | TDynamic
  | TFun Multiplicity Type Type
  | TRecord (Map Name Type)
  | TUnion [Type]
  | TApp Type Type
  | TForall [Name] Type
  | TConditional Type Type Type Type
  | TInfer Name
  deriving (Eq, Ord, Show)

-- | A user-facing polymorphic type scheme.
--
-- The checker closes remaining metas into synthetic type variables before it
-- returns a scheme. That makes CLI output, LSP hover, and declaration files
-- deterministic even when inference started from fresh unknowns.
data Scheme = Scheme {schemeVars :: [Name], schemeType :: Type}
  deriving (Eq, Ord, Show)

-- | A named type alias.
--
-- Aliases are intentionally expressive enough to encode generic helpers, HKT-
-- shaped encodings, and ambient library surfaces while still being easy to
-- expand structurally.
data TypeAlias = TypeAlias
  { typeAliasName :: Name,
    typeAliasParams :: [Name],
    typeAliasBody :: Type
  }
  deriving (Eq, Ord, Show)

tString, tInt, tFloat, tNumber, tNat, tBool, tNull, tPath, tDynamic :: Type
tString = TCon "String"
tInt = TCon "Int"
tFloat = TCon "Float"
tNumber = TCon "Number"
tNat = TCon "Nat"
tBool = TCon "Bool"
tNull = TCon "Null"
tPath = TCon "Path"
tDynamic = TDynamic

-- | Smart constructor for the built-in list type constructor.
tList :: Type -> Type
tList = TApp (TCon "List")

-- | Convert a parsed annotation into a scheme.
--
-- Source annotations may spell polymorphism directly via 'TForall'. Everywhere
-- else in the implementation we store polymorphism in 'Scheme', so this helper
-- is the boundary between those two representations.
schemeFromAnnotation :: Type -> Scheme
schemeFromAnnotation (TForall vars body) = Scheme vars body
schemeFromAnnotation ty = Scheme [] ty

-- | Remove explicit universal quantifiers from a type tree.
--
-- Structural operations such as alias expansion and subtyping compare the body
-- shape rather than a top-level syntactic binder wrapper, but nested
-- polymorphic fields must stay intact so ambient records can expose generic
-- members like `builtins.map`.
eraseForall :: Type -> Type
eraseForall = \case
  TForall _ body -> eraseForall body
  other -> other

freeTypeVars :: Type -> Set Name
freeTypeVars = \case
  TVar name -> Set.singleton name
  TTypeList items -> foldMap freeTypeVars items
  TFun _ a b -> freeTypeVars a <> freeTypeVars b
  TRecord fields -> foldMap freeTypeVars fields
  TUnion members -> foldMap freeTypeVars members
  TApp f x -> freeTypeVars f <> freeTypeVars x
  TForall vars body -> freeTypeVars body `Set.difference` Set.fromList vars
  TConditional a b c d -> foldMap freeTypeVars [a, b, c, d]
  _ -> Set.empty

-- | Collect unresolved inference metas.
--
-- This is used for occurs checks during unification and when closing a final
-- inferred type into a stable scheme.
freeMetas :: Type -> Set Int
freeMetas = \case
  TMeta n -> Set.singleton n
  TTypeList items -> foldMap freeMetas items
  TFun _ a b -> freeMetas a <> freeMetas b
  TRecord fields -> foldMap freeMetas fields
  TUnion members -> foldMap freeMetas members
  TApp f x -> freeMetas f <> freeMetas x
  TForall _ body -> freeMetas body
  TConditional a b c d -> foldMap freeMetas [a, b, c, d]
  _ -> Set.empty

-- | Convenience wrapper around 'freeMetas' for schemes.
freeMetasScheme :: Scheme -> Set Int
freeMetasScheme = freeMetas . schemeType

-- | Substitute universally-quantified variables inside a type.
--
-- Alias expansion, conditional-type pattern matching, and scheme
-- instantiation all route through this one operation so they share the same
-- binder-avoidance behavior.
substituteTypeVars :: Map Name Type -> Type -> Type
substituteTypeVars env = go
  where
    go = \case
      TVar name -> Map.findWithDefault (TVar name) name env
      TTypeList items -> TTypeList (go <$> items)
      TFun mult a b -> TFun mult (go a) (go b)
      TRecord fields -> TRecord (fmap go fields)
      TUnion members -> TUnion (go <$> members)
      TApp f x -> TApp (go f) (go x)
      TForall vars body -> TForall vars (substituteTypeVars (foldr Map.delete env vars) body)
      TConditional a b c d -> TConditional (go a) (go b) (go c) (go d)
      other -> other

-- | Substitute inference metas with their solved types.
--
-- Unlike 'substituteTypeVars', this walk recursively chases already-solved
-- metas so callers get a normalized view of the inference state.
substituteMetas :: Map Int Type -> Type -> Type
substituteMetas env = go
  where
    go = \case
      TMeta n -> maybe (TMeta n) go (Map.lookup n env)
      TTypeList items -> TTypeList (go <$> items)
      TFun mult a b -> TFun mult (go a) (go b)
      TRecord fields -> TRecord (fmap go fields)
      TUnion members -> TUnion (go <$> members)
      TApp f x -> TApp (go f) (go x)
      TForall vars body -> TForall vars (go body)
      TConditional a b c d -> TConditional (go a) (go b) (go c) (go d)
      other -> other

-- | Close remaining metas into a user-visible polymorphic scheme.
--
-- The resulting variable names are synthetic but deterministic (`t0`, `t1`,
-- ...). This mirrors how TypeScript surfaces fresh type variables in tooling
-- even when the source never wrote them explicitly.
closeMetas :: Type -> Scheme
closeMetas ty =
  let metas = sort (Set.toList (freeMetas ty))
      vars = [Text.pack ("t" <> show i) | i <- [0 .. length metas - 1]]
      subst = Map.fromList (zip metas (TVar <$> vars))
   in Scheme vars (substituteMetas subst ty)
