{-# LANGUAGE OverloadedStrings #-}

module Tnix.Parser.Expr (expressionParser, programParser) where

import Data.Text qualified as Text
import Text.Megaparsec
import Tnix.Parser.Lexer
import Tnix.Parser.Type
import Tnix.Syntax
import Tnix.Type

programParser :: Parser Program
programParser = do
  decls <- many declarationParser
  expr <- optional expressionParser
  pure
    Program
      { programAliases = [alias | Left alias <- decls],
        programAmbient = [ambient | Right ambient <- decls],
        programExpr = expr
      }

declarationParser :: Parser (Either TypeAlias AmbientDecl)
declarationParser = try (Left <$> aliasParser) <|> (Right <$> ambientParser)

aliasParser :: Parser TypeAlias
aliasParser = do
  reserved "type"
  name <- identifier
  params <- many identifier
  _ <- symbol "="
  body <- typeParser
  _ <- symbol ";"
  pure TypeAlias {typeAliasName = name, typeAliasParams = params, typeAliasBody = body}

ambientParser :: Parser AmbientDecl
ambientParser = do
  reserved "declare"
  path <- pathLiteral <|> (Text.unpack <$> stringLiteral)
  entries <- braces (many ambientEntry)
  _ <- symbol ";"
  pure AmbientDecl {ambientPath = path, ambientEntries = entries}

ambientEntry :: Parser AmbientEntry
ambientEntry = do
  name <- identifier
  _ <- symbol "::"
  ty <- typeParser
  _ <- symbol ";"
  pure AmbientEntry {ambientEntryName = name, ambientEntryType = ty}

expressionParser :: Parser Expr
expressionParser = choice [ifParser, letParser, try lambdaParser, applicationParser]

ifParser :: Parser Expr
ifParser = do
  reserved "if"
  cond <- expressionParser
  reserved "then"
  yesExpr <- expressionParser
  reserved "else"
  noExpr <- expressionParser
  pure (EIf cond yesExpr noExpr)

letParser :: Parser Expr
letParser = do
  reserved "let"
  items <- many letItemParser
  reserved "in"
  ELet items <$> expressionParser

letItemParser :: Parser LetItem
letItemParser = try sigParser <|> bindParser
  where
    sigParser = do
      name <- identifier
      _ <- symbol "::"
      ty <- typeParser
      _ <- symbol ";"
      pure (LetSignature name ty)
    bindParser = do
      name <- identifier
      _ <- symbol "="
      expr <- expressionParser
      _ <- symbol ";"
      pure (LetBinding name expr)

lambdaParser :: Parser Expr
lambdaParser = do
  pattern' <- patternParser
  _ <- symbol ":"
  ELambda pattern' <$> expressionParser

applicationParser :: Parser Expr
applicationParser = foldl1 EApp <$> some postfixParser

postfixParser :: Parser Expr
postfixParser = do
  base <- atomParser
  fields <- many (try (symbol "." *> identifier))
  pure $ if null fields then base else ESelect base fields

atomParser :: Parser Expr
atomParser =
  choice
    [ parens expressionParser,
      attrSetParser,
      listParser,
      EString <$> stringLiteral,
      EInt <$> integer,
      EBool True <$ reserved "true",
      EBool False <$ reserved "false",
      ENull <$ reserved "null",
      EPath <$> pathLiteral,
      EVar <$> identifier
    ]

attrSetParser :: Parser Expr
attrSetParser = EAttrSet <$> braces (many attrParser)

attrParser :: Parser AttrItem
attrParser = try inheritParser <|> fieldParser
  where
    inheritParser = reserved "inherit" *> (AttrInherit <$> some identifier) <* symbol ";"
    fieldParser = do
      name <- identifier
      _ <- symbol "="
      expr <- expressionParser
      _ <- symbol ";"
      pure (AttrField name expr)

listParser :: Parser Expr
listParser = EList <$> brackets (many listItem)
  where
    listItem = choice [ifParser, letParser, try lambdaParser, postfixParser]

patternParser :: Parser Pattern
patternParser = parens typed <|> (PVar <$> identifier <*> pure Nothing)
  where
    typed = do
      name <- identifier
      _ <- symbol "::"
      PVar name . Just <$> typeParser
