{-# LANGUAGE OverloadedStrings #-}

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

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser ()
reserved word = lexeme $ try $ string word *> notFollowedBy (satisfy identCont)

identifier :: Parser Name
identifier = lexeme $ try $ do
  first <- satisfy identStart
  rest <- many (satisfy identCont)
  let name = Text.pack (first : rest)
  when (name `elem` reservedWords) (fail "reserved word")
  pure name

stringLiteral :: Parser Text
stringLiteral = lexeme $ Text.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

integer :: Parser Integer
integer = lexeme L.decimal

pathLiteral :: Parser FilePath
pathLiteral = lexeme $ try $ do
  prefix <- Text.unpack <$> choice [string "../", string "./", string "/"]
  rest <- many (satisfy pathChar)
  pure (prefix <> rest)

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
