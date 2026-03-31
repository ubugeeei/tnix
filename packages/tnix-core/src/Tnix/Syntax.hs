module Tnix.Syntax
  ( AmbientDecl (..),
    AmbientEntry (..),
    AttrItem (..),
    Expr (..),
    LetItem (..),
    Pattern (..),
    Program (..),
  )
where

import Tnix.Type (Name, Type, TypeAlias)

data Program = Program
  { programAliases :: [TypeAlias],
    programAmbient :: [AmbientDecl],
    programExpr :: Maybe Expr
  }
  deriving (Eq, Show)

data AmbientDecl = AmbientDecl
  { ambientPath :: FilePath,
    ambientEntries :: [AmbientEntry]
  }
  deriving (Eq, Show)

data AmbientEntry = AmbientEntry
  { ambientEntryName :: Name,
    ambientEntryType :: Type
  }
  deriving (Eq, Show)

data Expr
  = EVar Name
  | EString Name
  | EInt Integer
  | EBool Bool
  | ENull
  | EPath FilePath
  | ELambda Pattern Expr
  | EApp Expr Expr
  | ELet [LetItem] Expr
  | EAttrSet [AttrItem]
  | ESelect Expr [Name]
  | EIf Expr Expr Expr
  | EList [Expr]
  deriving (Eq, Show)

data Pattern = PVar Name (Maybe Type)
  deriving (Eq, Show)

data LetItem
  = LetSignature Name Type
  | LetBinding Name Expr
  deriving (Eq, Show)

data AttrItem
  = AttrField Name Expr
  | AttrInherit [Name]
  deriving (Eq, Show)
