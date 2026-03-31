-- | Surface syntax for tnix.
--
-- The AST intentionally stays close to ordinary Nix source. Type-only syntax is
-- attached as annotations or top-level declarations so that erasure back to
-- `.nix` is straightforward and existing mental models still apply.
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

-- | A complete tnix file.
--
-- A file may contain type aliases, ambient declarations, and optionally a root
-- expression. Declaration-only files model `.d.tnix` surfaces, while ordinary
-- `.tnix` files typically contain all three sections in varying combinations.
data Program = Program
  { programAliases :: [TypeAlias],
    programAmbient :: [AmbientDecl],
    programExpr :: Maybe Expr
  }
  deriving (Eq, Show)

-- | Ambient declaration describing an existing `.nix` file.
--
-- This is the bridge that lets tnix add types to code it does not compile
-- itself. The declaration path is resolved relative to the file that declared
-- it, matching how Nix imports are usually written.
data AmbientDecl = AmbientDecl
  { ambientPath :: FilePath,
    ambientEntries :: [AmbientEntry]
  }
  deriving (Eq, Show)

-- | A single exported member inside an ambient declaration.
data AmbientEntry = AmbientEntry
  { ambientEntryName :: Name,
    ambientEntryType :: Type
  }
  deriving (Eq, Show)

-- | Term-level expressions preserved by the compiler.
--
-- The set is intentionally small and currently covers the subset needed to
-- prove the architecture: records, lambdas, applications, imports, selections,
-- lists, and local bindings.
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

-- | Lambda binder pattern.
--
-- tnix currently supports variable binders with an optional annotation. The
-- syntax is deliberately Haskell-like while remaining valid-looking to Nix
-- users.
data Pattern = PVar Name (Maybe Type)
  deriving (Eq, Show)

-- | Items allowed in a `let` block.
--
-- Signatures are erased before compilation but kept during checking and
-- declaration emission.
data LetItem
  = LetSignature Name Type
  | LetBinding Name Expr
  deriving (Eq, Show)

-- | Record attributes inside an attribute set.
--
-- `inherit` is modeled explicitly so the checker can resolve inherited names
-- instead of flattening them away during parsing.
data AttrItem
  = AttrField Name Expr
  | AttrInherit [Name]
  deriving (Eq, Show)
