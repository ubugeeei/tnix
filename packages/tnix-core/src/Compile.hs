-- | Erasure-based compiler from `.tnix` to `.nix`.
--
-- The compiler does not change runtime semantics. It simply removes type-only
-- constructs, leaving a Nix expression that stays close to the original source
-- layout.
module Compile (compileProgram) where

import Data.Text (Text)
import Pretty (renderProgramAsNix)
import Syntax

-- | Compile a checked or unchecked program by erasing type syntax first.
compileProgram :: Program -> Either Text Text
compileProgram = renderProgramAsNix . eraseProgram

eraseProgram :: Program -> Program
eraseProgram program =
  program
    { programExpr = fmap (\marked -> marked {markedValue = eraseExpr (markedValue marked)}) (programExpr program)
    }

eraseExpr :: Expr -> Expr
eraseExpr expr =
  case expr of
    ELambda pattern' body -> ELambda (erasePattern pattern') (eraseExpr body)
    EApp fun arg -> EApp (eraseExpr fun) (eraseExpr arg)
    ELet items body -> ELet (map eraseMarkedLetItem [item | item <- items, isLetBinding (markedValue item)]) (eraseExpr body)
    EAttrSet items -> EAttrSet (map eraseAttrItem items)
    ESelect base fields -> ESelect (eraseExpr base) fields
    EIf cond yesExpr noExpr -> EIf (eraseExpr cond) (eraseExpr yesExpr) (eraseExpr noExpr)
    EList members -> EList (map eraseExpr members)
    ECast inner _ -> eraseExpr inner
    other -> other

erasePattern :: Pattern -> Pattern
erasePattern (PVar name _) = PVar name Nothing

eraseLetItem :: LetItem -> LetItem
eraseLetItem (LetBinding name expr) = LetBinding name (eraseExpr expr)
eraseLetItem item = item

eraseMarkedLetItem :: Marked LetItem -> Marked LetItem
eraseMarkedLetItem marked = marked {markedValue = eraseLetItem (markedValue marked)}

isLetBinding :: LetItem -> Bool
isLetBinding LetBinding {} = True
isLetBinding _ = False

eraseAttrItem :: AttrItem -> AttrItem
eraseAttrItem item =
  case item of
    AttrField name expr -> AttrField name (eraseExpr expr)
    AttrInherit names -> AttrInherit names
