{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Tnix.Type
  ( LiteralType (..),
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
    tInt,
    tList,
    tNull,
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

type Name = Text

data LiteralType = LBool Bool | LInt Integer | LString Text
  deriving (Eq, Ord, Show)

data Type
  = TVar Name
  | TCon Name
  | TMeta Int
  | TLit LiteralType
  | TDynamic
  | TFun Type Type
  | TRecord (Map Name Type)
  | TUnion [Type]
  | TApp Type Type
  | TForall [Name] Type
  | TConditional Type Type Type Type
  | TInfer Name
  deriving (Eq, Ord, Show)

data Scheme = Scheme {schemeVars :: [Name], schemeType :: Type}
  deriving (Eq, Ord, Show)

data TypeAlias = TypeAlias
  { typeAliasName :: Name,
    typeAliasParams :: [Name],
    typeAliasBody :: Type
  }
  deriving (Eq, Ord, Show)

tString, tInt, tBool, tNull, tPath, tDynamic :: Type
tString = TCon "String"
tInt = TCon "Int"
tBool = TCon "Bool"
tNull = TCon "Null"
tPath = TCon "Path"
tDynamic = TDynamic

tList :: Type -> Type
tList = TApp (TCon "List")

schemeFromAnnotation :: Type -> Scheme
schemeFromAnnotation (TForall vars body) = Scheme vars body
schemeFromAnnotation ty = Scheme [] ty

eraseForall :: Type -> Type
eraseForall = \case
  TForall _ body -> eraseForall body
  TFun a b -> TFun (eraseForall a) (eraseForall b)
  TRecord fields -> TRecord (fmap eraseForall fields)
  TUnion members -> TUnion (eraseForall <$> members)
  TApp f x -> TApp (eraseForall f) (eraseForall x)
  TConditional a b c d -> TConditional (eraseForall a) (eraseForall b) (eraseForall c) (eraseForall d)
  other -> other

freeTypeVars :: Type -> Set Name
freeTypeVars = \case
  TVar name -> Set.singleton name
  TFun a b -> freeTypeVars a <> freeTypeVars b
  TRecord fields -> foldMap freeTypeVars fields
  TUnion members -> foldMap freeTypeVars members
  TApp f x -> freeTypeVars f <> freeTypeVars x
  TForall vars body -> freeTypeVars body `Set.difference` Set.fromList vars
  TConditional a b c d -> foldMap freeTypeVars [a, b, c, d]
  _ -> Set.empty

freeMetas :: Type -> Set Int
freeMetas = \case
  TMeta n -> Set.singleton n
  TFun a b -> freeMetas a <> freeMetas b
  TRecord fields -> foldMap freeMetas fields
  TUnion members -> foldMap freeMetas members
  TApp f x -> freeMetas f <> freeMetas x
  TForall _ body -> freeMetas body
  TConditional a b c d -> foldMap freeMetas [a, b, c, d]
  _ -> Set.empty

freeMetasScheme :: Scheme -> Set Int
freeMetasScheme = freeMetas . schemeType

substituteTypeVars :: Map Name Type -> Type -> Type
substituteTypeVars env = go
  where
    go = \case
      TVar name -> Map.findWithDefault (TVar name) name env
      TFun a b -> TFun (go a) (go b)
      TRecord fields -> TRecord (fmap go fields)
      TUnion members -> TUnion (go <$> members)
      TApp f x -> TApp (go f) (go x)
      TForall vars body -> TForall vars (substituteTypeVars (foldr Map.delete env vars) body)
      TConditional a b c d -> TConditional (go a) (go b) (go c) (go d)
      other -> other

substituteMetas :: Map Int Type -> Type -> Type
substituteMetas env = go
  where
    go = \case
      TMeta n -> maybe (TMeta n) go (Map.lookup n env)
      TFun a b -> TFun (go a) (go b)
      TRecord fields -> TRecord (fmap go fields)
      TUnion members -> TUnion (go <$> members)
      TApp f x -> TApp (go f) (go x)
      TForall vars body -> TForall vars (go body)
      TConditional a b c d -> TConditional (go a) (go b) (go c) (go d)
      other -> other

closeMetas :: Type -> Scheme
closeMetas ty =
  let metas = sort (Set.toList (freeMetas ty))
      vars = [Text.pack ("t" <> show i) | i <- [0 .. length metas - 1]]
      subst = Map.fromList (zip metas (TVar <$> vars))
   in Scheme vars (substituteMetas subst ty)
