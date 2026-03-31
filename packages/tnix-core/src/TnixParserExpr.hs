{-# LANGUAGE OverloadedStrings #-}

-- | Parser for top-level declarations and executable expressions.
--
-- The parser preserves Nix-like surface structure as much as possible so that
-- compilation can be implemented as a mostly mechanical erasure pass.
module TnixParserExpr (expressionParser, programParser) where

import Data.Text qualified as Text
import Text.Megaparsec
import TnixParserLexer
import TnixParserType
import TnixSyntax
import TnixType

-- | Parse a full tnix source file.
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

-- | Parse either a type alias or an ambient declaration.
declarationParser :: Parser (Either TypeAlias AmbientDecl)
declarationParser = try (Left <$> aliasParser) <|> (Right <$> ambientParser)

-- | Parse a top-level `type` alias declaration.
aliasParser :: Parser TypeAlias
aliasParser = do
  reserved "type"
  name <- identifier
  params <- many identifier
  _ <- symbol "="
  body <- typeParser
  _ <- symbol ";"
  pure TypeAlias {typeAliasName = name, typeAliasParams = params, typeAliasBody = body}

-- | Parse a `declare` block that describes an existing `.nix` module.
ambientParser :: Parser AmbientDecl
ambientParser = do
  reserved "declare"
  path <- pathLiteral <|> (Text.unpack <$> stringLiteral)
  entries <- braces (many ambientEntry)
  _ <- symbol ";"
  pure AmbientDecl {ambientPath = path, ambientEntries = entries}

-- | Parse a single ambiently-exported member.
ambientEntry :: Parser AmbientEntry
ambientEntry = do
  name <- identifier
  _ <- symbol "::"
  ty <- typeParser
  _ <- symbol ";"
  pure AmbientEntry {ambientEntryName = name, ambientEntryType = ty}

-- | Parse any expression form supported by the prototype.
expressionParser :: Parser Expr
expressionParser = choice [ifParser, letParser, try lambdaParser, applicationParser]

-- | Parse a Nix-style conditional expression.
ifParser :: Parser Expr
ifParser = do
  reserved "if"
  cond <- expressionParser
  reserved "then"
  yesExpr <- expressionParser
  reserved "else"
  noExpr <- expressionParser
  pure (EIf cond yesExpr noExpr)

-- | Parse a `let ... in ...` block with optional type signatures.
letParser :: Parser Expr
letParser = do
  reserved "let"
  items <- many letItemParser
  reserved "in"
  ELet items <$> expressionParser

-- | Parse either a type signature or a value binding inside a `let`.
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

-- | Parse a lambda using tnix's Haskell-like binder syntax.
lambdaParser :: Parser Expr
lambdaParser = do
  pattern' <- patternParser
  _ <- symbol ":"
  ELambda pattern' <$> expressionParser

-- | Parse left-associated application chains.
applicationParser :: Parser Expr
applicationParser = foldl1 EApp <$> some postfixParser

-- | Parse postfix field selections without stealing path literals.
postfixParser :: Parser Expr
postfixParser = do
  base <- atomParser
  fields <- many (try (symbol "." *> identifier))
  pure $ if null fields then base else ESelect base fields

-- | Parse atomic expression forms.
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

-- | Parse an attribute set.
attrSetParser :: Parser Expr
attrSetParser = EAttrSet <$> braces (many attrParser)

-- | Parse either an explicit field or an `inherit` clause.
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

-- | Parse a list literal.
listParser :: Parser Expr
listParser = EList <$> brackets (many listItem)
  where
    listItem = choice [ifParser, letParser, try lambdaParser, postfixParser]

-- | Parse a lambda binder pattern with an optional inline annotation.
patternParser :: Parser Pattern
patternParser = parens typed <|> (PVar <$> identifier <*> pure Nothing)
  where
    typed = do
      name <- identifier
      _ <- symbol "::"
      PVar name . Just <$> typeParser
