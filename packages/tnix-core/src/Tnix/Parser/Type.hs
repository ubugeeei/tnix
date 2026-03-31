{-# LANGUAGE OverloadedStrings #-}

module Tnix.Parser.Type (typeParser) where

import Data.Map.Strict qualified as Map
import Text.Megaparsec
import Tnix.Parser.Lexer
import Tnix.Type

typeParser :: Parser Type
typeParser = forallParser <|> conditionalParser

forallParser :: Parser Type
forallParser = try $ do
  reserved "forall"
  vars <- some identifier
  _ <- symbol "."
  TForall vars <$> typeParser

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

functionParser :: Parser Type
functionParser = foldr1 TFun <$> sepBy1 unionParser (symbol "->")

unionParser :: Parser Type
unionParser = mkUnion <$> sepBy1 appParser (symbol "|")
  where
    mkUnion [oneTy] = oneTy
    mkUnion manyTypes = TUnion manyTypes

appParser :: Parser Type
appParser = foldl1 TApp <$> some atomParser

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

recordParser :: Parser Type
recordParser = TRecord . Map.fromList <$> braces (many fieldParser)
  where
    fieldParser = do
      name <- identifier
      _ <- symbol "::"
      ty <- typeParser
      _ <- symbol ";"
      pure (name, ty)

inferParser :: Parser Type
inferParser = reserved "infer" *> (TInfer <$> identifier)

varOrConParser :: Parser Type
varOrConParser = do
  name <- identifier
  pure $
    case show name of
      ('"' : c : _) | c >= 'A' && c <= 'Z' -> TCon name
      _ -> TVar name
