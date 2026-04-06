{-# LANGUAGE OverloadedStrings #-}

-- | Parser for tnix type syntax.
--
-- The syntax combines Haskell-like binders (`forall`) with TypeScript-inspired
-- features (`extends`, `infer`) while keeping the visual shape light enough to
-- sit next to ordinary Nix code.
module ParserType (typeParser) where

import Data.Char (isUpper)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Text.Megaparsec
import ParserLexer
import Type

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
functionParser = do
  lhs <- unionParser
  option lhs $ do
    mult <- arrowMultiplicityParser
    rhs <- functionParser
    pure (TFun mult lhs rhs)

arrowMultiplicityParser :: Parser Multiplicity
arrowMultiplicityParser =
  choice
    [ One <$ try (symbol "%1" *> symbol "->"),
      Many <$ symbol "->"
    ]

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
    [ typeListParser,
      recordParser,
      TLit . LString <$> stringLiteral,
      TLit . LFloat <$> float,
      TLit . LInt <$> integer,
      TLit (LBool True) <$ reserved "true",
      TLit (LBool False) <$ reserved "false",
      TAny <$ reserved "any",
      TDynamic <$ reserved "dynamic",
      TUnknown <$ reserved "unknown",
      inferParser,
      parens typeParser,
      varOrConParser
    ]

-- | Parse structural record types.
recordParser :: Parser Type
recordParser = TRecord . Map.fromList <$> braces (many fieldParser)
  where
    fieldParser = do
      name <- attrName
      _ <- symbol "::"
      ty <- typeParser
      _ <- symbol ";"
      pure (name, ty)

-- | Parse type-level shape lists such as `[2 3 4]`.
typeListParser :: Parser Type
typeListParser = TTypeList <$> brackets (many shapeItemParser)

shapeItemParser :: Parser Type
shapeItemParser =
  choice
    [ TLit . LString <$> stringLiteral,
      TLit . LFloat <$> float,
      TLit . LInt <$> integer,
      TLit (LBool True) <$ reserved "true",
      TLit (LBool False) <$ reserved "false",
      TAny <$ reserved "any",
      TDynamic <$ reserved "dynamic",
      TUnknown <$ reserved "unknown",
      inferParser,
      parens typeParser,
      varOrConParser
    ]

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
    case name of
      _
        | Just (c, _) <- Text.uncons name,
          isUpper c ->
            TCon name
      _ -> TVar name
