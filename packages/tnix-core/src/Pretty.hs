{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pretty-printers for emitted `.nix`, `.d.tnix`, and human-facing type text.
--
-- One module owns all rendering so that CLI output, declaration files, and
-- debug/test expectations share the same surface representation.
module Pretty
  ( renderDeclarationFile,
    renderExpr,
    renderProgramAsNix,
    renderScheme,
    renderType,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Indexed (tensorView, tupleView)
import Prettyprinter
import Prettyprinter.Render.Text qualified as Render
import Syntax
import Type

-- | Render an executable program back to plain Nix code.
renderProgramAsNix :: Program -> Either Text Text
renderProgramAsNix program =
  maybe (Left "declaration-only files cannot be compiled to .nix") (Right . render . prettyExpr 0 . markedValue) (programExpr program)

-- | Render a declaration file for a target path and exported entries.
renderDeclarationFile :: FilePath -> [TypeAlias] -> [(Name, Type)] -> Text
renderDeclarationFile path aliases entries = render $ vsep (map prettyAlias aliases <> [prettyDecl path entries])

-- | Render an expression using tnix/Nix surface syntax.
renderExpr :: Expr -> Text
renderExpr = render . prettyExpr 0

-- | Render a type without scheme quantifiers.
renderType :: Type -> Text
renderType = render . prettyType 0

-- | Render a polymorphic scheme for CLI and LSP display.
renderScheme :: Scheme -> Text
renderScheme (Scheme vars ty) =
  render $
    if null vars
      then prettyType 0 ty
      else "forall" <+> hsep (pretty <$> vars) <> "." <+> prettyType 0 ty

render :: Doc ann -> Text
render = Render.renderStrict . layoutPretty defaultLayoutOptions

prettyAlias :: TypeAlias -> Doc ann
prettyAlias alias =
  "type"
    <+> pretty (typeAliasName alias)
    <+> hsep (pretty <$> typeAliasParams alias)
    <+> "="
    <+> prettyType 0 (typeAliasBody alias)
    <> ";"

prettyDecl :: FilePath -> [(Name, Type)] -> Doc ann
prettyDecl path entries =
  vsep
    [ "declare" <+> dquotes (pretty path) <+> "{",
      indent 2 (vsep [pretty name <+> "::" <+> prettyType 0 ty <> ";" | (name, ty) <- entries]),
      "};"
    ]

prettyExpr :: Int -> Expr -> Doc ann
prettyExpr p = \case
  EVar name -> pretty name
  EString value -> dquotes (pretty value)
  EFloat value -> pretty (show value)
  EInt value -> pretty value
  EBool True -> "true"
  EBool False -> "false"
  ENull -> "null"
  EPath path -> pretty path
  ELambda (PVar name _) body -> parenIf (p > 0) (pretty name <> ":" <+> prettyExpr 0 body)
  EApp f x -> parenIf (p > 1) (prettyExpr 1 f <+> prettyExpr 2 x)
  EAdd left right -> parenIf (p > 0) (prettyExpr 1 left <+> "+" <+> prettyExpr 1 right)
  ELet items body -> vsep ["let", indent 2 (vsep (map (prettyLet . markedValue) items)), "in" <+> prettyExpr 0 body]
  EAttrSet items -> vsep ["{", indent 2 (vsep (map prettyAttr items)), "}"]
  ESelect base names -> parenIf (p > 2) (prettyExpr 2 base <> foldMap (("." <>) . pretty) names)
  EIf a b c -> vsep ["if" <+> prettyExpr 0 a, "then" <+> prettyExpr 0 b, "else" <+> prettyExpr 0 c]
  EList items -> "[" <+> hsep (map (prettyExpr 0) items) <+> "]"
  ECast expr ty -> parenIf (p > 0) (prettyExpr 1 expr <+> "as" <+> prettyType 0 ty)

prettyLet :: LetItem -> Doc ann
prettyLet = \case
  LetSignature name ty -> pretty name <+> "::" <+> prettyType 0 ty <> ";"
  LetBinding name expr -> pretty name <+> "=" <+> prettyExpr 0 expr <> ";"

prettyAttr :: AttrItem -> Doc ann
prettyAttr = \case
  AttrField name expr -> pretty name <+> "=" <+> prettyExpr 0 expr <> ";"
  AttrInherit names -> "inherit" <+> hsep (pretty <$> names) <> ";"

prettyType :: Int -> Type -> Doc ann
prettyType p ty =
  case tupleView ty of
    Just items -> parenIf (p > 2) ("Tuple" <+> prettyType 3 (TTypeList items))
    Nothing ->
      case tensorView ty of
        Just (dims, elemTy) ->
          case dims of
            [lenTy] -> parenIf (p > 2) ("Vec" <+> prettyType 3 lenTy <+> prettyType 3 elemTy)
            [rowsTy, colsTy] -> parenIf (p > 2) ("Matrix" <+> prettyType 3 rowsTy <+> prettyType 3 colsTy <+> prettyType 3 elemTy)
            _ -> parenIf (p > 2) ("Tensor" <+> prettyType 3 (TTypeList dims) <+> prettyType 3 elemTy)
        Nothing ->
          case ty of
            TVar name -> pretty name
            TCon name -> pretty name
            TMeta n -> pretty ("?" <> show n)
            TLit (LString text) -> dquotes (pretty text)
            TLit (LFloat n) -> pretty (show n)
            TLit (LInt n) -> pretty n
            TLit (LBool True) -> "true"
            TLit (LBool False) -> "false"
            TAny -> "any"
            TTypeList items -> "[" <+> hsep (prettyType 3 <$> items) <+> "]"
            TDynamic -> "dynamic"
            TUnknown -> "unknown"
            TFun mult a b ->
              let arrow =
                    case mult of
                      One -> "%1 ->"
                      Many -> "->"
               in parenIf (p > 0) (prettyType 1 a <+> arrow <+> prettyType 0 b)
            TRecord fields -> vsep ["{", indent 2 (vsep [pretty k <+> "::" <+> prettyType 0 v <> ";" | (k, v) <- Map.toList fields]), "}"]
            TUnion members -> parenIf (p > 1) (hsep (punctuate " |" (map (prettyType 2) members)))
            TApp f x -> parenIf (p > 2) (prettyType 2 f <+> prettyType 3 x)
            TForall vars body -> parenIf (p > 0) ("forall" <+> hsep (pretty <$> vars) <> "." <+> prettyType 0 body)
            TConditional a b c d -> parenIf (p > 0) (prettyType 2 a <+> "extends" <+> prettyType 2 b <+> "?" <+> prettyType 0 c <+> ":" <+> prettyType 0 d)
            TInfer name -> "infer" <+> pretty name

parenIf :: Bool -> Doc ann -> Doc ann
parenIf True = parens
parenIf False = id
