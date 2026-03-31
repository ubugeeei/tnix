{-# LANGUAGE OverloadedStrings #-}

module Tnix.Emit (emitDeclarationFile) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.FilePath (replaceExtension, takeFileName)
import Tnix.Pretty (renderDeclarationFile)
import Tnix.Syntax
import Tnix.Type

emitDeclarationFile :: FilePath -> Program -> Scheme -> Text
emitDeclarationFile source program scheme =
  renderDeclarationFile (("./" <>) (replaceExtension (takeFileName source) "nix")) (programAliases program) entries
  where
    entries =
      case (programExpr program, schemeType scheme, schemeVars scheme) of
        (Just (EAttrSet _), TRecord fields, []) -> Map.toList fields
        _ -> [("default", quantified)]
    quantified =
      case schemeVars scheme of
        [] -> schemeType scheme
        _ -> TForall (schemeVars scheme) (schemeType scheme)
