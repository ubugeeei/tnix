module Tnix.Compile (compileProgram) where

import Data.Text (Text)
import Tnix.Pretty (renderProgramAsNix)
import Tnix.Syntax

compileProgram :: Program -> Either Text Text
compileProgram = renderProgramAsNix . eraseProgram

eraseProgram :: Program -> Program
eraseProgram program =
  program
    { programExpr = eraseExpr <$> programExpr program
    }

eraseExpr :: Expr -> Expr
eraseExpr expr =
  case expr of
    ELambda pattern' body -> ELambda (erasePattern pattern') (eraseExpr body)
    EApp fun arg -> EApp (eraseExpr fun) (eraseExpr arg)
    ELet items body -> ELet (map eraseLetItem [item | item@LetBinding {} <- items]) (eraseExpr body)
    EAttrSet items -> EAttrSet (map eraseAttrItem items)
    ESelect base fields -> ESelect (eraseExpr base) fields
    EIf cond yesExpr noExpr -> EIf (eraseExpr cond) (eraseExpr yesExpr) (eraseExpr noExpr)
    EList members -> EList (map eraseExpr members)
    other -> other

erasePattern :: Pattern -> Pattern
erasePattern (PVar name _) = PVar name Nothing

eraseLetItem :: LetItem -> LetItem
eraseLetItem (LetBinding name expr) = LetBinding name (eraseExpr expr)
eraseLetItem item = item

eraseAttrItem :: AttrItem -> AttrItem
eraseAttrItem item =
  case item of
    AttrField name expr -> AttrField name (eraseExpr expr)
    AttrInherit names -> AttrInherit names
