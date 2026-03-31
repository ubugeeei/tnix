{-# LANGUAGE OverloadedStrings #-}

-- | Parser for tnix type syntax.
--
-- The syntax combines Haskell-like binders (`forall`) with TypeScript-inspired
-- features (`extends`, `infer`) while keeping the visual shape light enough to
-- sit next to ordinary Nix code.
module Tnix.Parser.Type (typeParser) where

import Data.Map.Strict qualified as Map
import Text.Megaparsec
import Tnix.Parser.Lexer
import Tnix.Type

-- | Entry point for type parsing.
typeParser :: Parser Type
typeParser = forallParser <|> conditionalParser

-- | Parse explicit universal quantification.
forallParser :: Parser Type
forallParser = try $ do
  reserved "forall"
  vars <- some identifier
  _ <- symbol "."
  TForall vars <$> typeParser

-- | Parse conditional types of the form `A extends B ? C : D`.
conditionalParser :: Parser Type
conditionalParser = do
  lhs <- functionParser
  option lhs $ do
    reserved "extends"
    rhs <- functionParser
    _ <- symbol "?"
    yesTy <- typeParser
    _ <- symbol ":"
    noTy <- typeParser
    pure (TConditional lhs rhs yesTy noTy)

-- | Parse right-associative function arrows.
functionParser :: Parser Type
functionParser = foldr1 TFun <$> sepBy1 unionParser (symbol "->")

-- | Parse normalized unions.
unionParser :: Parser Type
unionParser = mkUnion <$> sepBy1 appParser (symbol "|")
  where
    mkUnion [oneTy] = oneTy
    mkUnion manyTypes = TUnion manyTypes

-- | Parse left-associated type applications.
appParser :: Parser Type
appParser = foldl1 TApp <$> some atomParser

-- | Parse atomic type forms.
atomParser :: Parser Type
atomParser =
  choice
    [ recordParser,
      TLit . LString <$> stringLiteral,
      TLit . LInt <$> integer,
      TLit (LBool True) <$ reserved "true",
      TLit (LBool False) <$ reserved "false",
      TDynamic <$ reserved "dynamic",
      inferParser,
      parens typeParser,
      varOrConParser
    ]

-- | Parse structural record types.
recordParser :: Parser Type
recordParser = TRecord . Map.fromList <$> braces (many fieldParser)
  where
    fieldParser = do
      name <- identifier
      _ <- symbol "::"
      ty <- typeParser
      _ <- symbol ";"
      pure (name, ty)

-- | Parse an `infer` binder used inside conditional-type patterns.
inferParser :: Parser Type
inferParser = reserved "infer" *> (TInfer <$> identifier)

-- | Parse either a constructor-like name or a type variable.
--
-- Uppercase-leading identifiers are treated as constructors so users can write
-- aliases that feel familiar to both Haskell and TypeScript audiences.
varOrConParser :: Parser Type
varOrConParser = do
  name <- identifier
  pure $
    case show name of
      ('"' : c : _) | c >= 'A' && c <= 'Z' -> TCon name
      _ -> TVar name
