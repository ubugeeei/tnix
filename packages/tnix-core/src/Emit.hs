{-# LANGUAGE OverloadedStrings #-}

-- | Declaration-file emitter.
--
-- Emission is intentionally structural: attribute-set roots become named
-- members, while everything else is exposed as `default`. This mirrors how
-- ambient declarations describe existing `.nix` modules.
module Emit (emitDeclarationFile) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.FilePath (replaceExtension, takeFileName)
import Pretty (renderDeclarationFile)
import Syntax
import Type

-- | Emit a `.d.tnix`-style declaration text for a source file.
--
-- Local aliases are preserved verbatim so downstream files can reuse the same
-- abstractions rather than only seeing fully-expanded record shapes.
emitDeclarationFile :: FilePath -> Program -> Scheme -> Text
emitDeclarationFile source program scheme =
  renderDeclarationFile (("./" <>) (replaceExtension (takeFileName source) "nix")) (programAliases program) entries
  where
    entries =
      case (markedValue <$> programExpr program, schemeType scheme, schemeVars scheme) of
        (Just (EAttrSet _), TRecord fields, []) -> Map.toList fields
        _ -> [("default", quantified)]
    quantified =
      case schemeVars scheme of
        [] -> schemeType scheme
        _ -> TForall (schemeVars scheme) (schemeType scheme)
