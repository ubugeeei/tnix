{-# LANGUAGE OverloadedStrings #-}

-- | Shared lexical layer for tnix parsers.
--
-- The grammar stays intentionally close to Nix, so the lexer mostly handles
-- whitespace, comments, simple identifiers, and path literals instead of
-- inventing a heavy token stream.
module Tnix.Parser.Lexer
  ( Parser,
    brackets,
    braces,
    identifier,
    integer,
    lexeme,
    parens,
    pathLiteral,
    reserved,
    sc,
    stringLiteral,
    symbol,
  )
where

import Control.Monad (when)
import Data.Char (isAlphaNum, isLetter)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Tnix.Type (Name)

-- | Parser type used throughout the frontend.
type Parser = Parsec Void Text

-- | Space consumer matching Nix-style comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

-- | Attach trailing space consumption to a parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a fixed symbol and consume following layout.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a reserved keyword while ensuring it is not a longer identifier
-- prefix.
reserved :: Text -> Parser ()
reserved word = lexeme $ try $ string word *> notFollowedBy (satisfy identCont)

-- | Parse a non-keyword identifier.
identifier :: Parser Name
identifier = lexeme $ try $ do
  first <- satisfy identStart
  rest <- many (satisfy identCont)
  let name = Text.pack (first : rest)
  when (name `elem` reservedWords) (fail "reserved word")
  pure name

-- | Parse a double-quoted string literal.
stringLiteral :: Parser Text
stringLiteral = lexeme $ Text.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- | Parse a decimal integer literal.
integer :: Parser Integer
integer = lexeme L.decimal

-- | Parse a Nix path literal such as `./foo.nix` or `/etc/hosts`.
pathLiteral :: Parser FilePath
pathLiteral = lexeme $ try $ do
  prefix <- Text.unpack <$> choice [string "../", string "./", string "/"]
  rest <- many (satisfy pathChar)
  pure (prefix <> rest)

-- | Standard bracket helpers used by both term and type parsers.
parens, braces, brackets :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

reservedWords :: [Text]
reservedWords =
  ["declare", "dynamic", "else", "extends", "false", "forall", "if", "in", "infer", "inherit", "let", "null", "then", "true", "type"]

identStart, identCont, pathChar :: Char -> Bool
identStart c = isLetter c || c == '_'
identCont c = isAlphaNum c || c `elem` ("_'-" :: String)
pathChar c = isAlphaNum c || c `elem` ("._/-+" :: String)
