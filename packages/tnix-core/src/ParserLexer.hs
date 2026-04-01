{-# LANGUAGE OverloadedStrings #-}

-- | Shared lexical layer for tnix parsers.
--
-- The grammar stays intentionally close to Nix, so the lexer mostly handles
-- whitespace, comments, simple identifiers, and path literals instead of
-- inventing a heavy token stream.
module ParserLexer
  ( DirectiveTargets,
    Parser,
    brackets,
    directiveForCurrentLine,
    braces,
    float,
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
import Control.Monad.Reader (ReaderT, asks)
import Data.Char (isAlphaNum, isLetter)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Syntax (DiagnosticDirective)
import Type (Name)

-- | Parser type used throughout the frontend.
type DirectiveTargets = Map Int DiagnosticDirective
type Parser = ReaderT DirectiveTargets (Parsec Void Text)

-- | Look up whether the current source line is targeted by a directive comment.
directiveForCurrentLine :: Parser (Maybe DiagnosticDirective)
directiveForCurrentLine = do
  lineNo <- unPos . sourceLine <$> getSourcePos
  asks (Map.lookup lineNo)

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

-- | Parse a decimal float literal with a required fractional part.
float :: Parser Double
float = lexeme . try $ do
  whole <- some digitChar
  _ <- char '.'
  frac <- some digitChar
  expo <- optional exponentPart
  pure (read (whole <> "." <> frac <> maybe "" id expo))
  where
    exponentPart = do
      marker <- oneOf ("eE" :: String)
      sign <- optional (oneOf ("+-" :: String))
      digits <- some digitChar
      pure (marker : maybe digits (: digits) sign)

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
  ["any", "declare", "dynamic", "else", "extends", "false", "forall", "if", "in", "infer", "inherit", "let", "null", "then", "true", "type", "unknown"]

identStart, identCont, pathChar :: Char -> Bool
identStart c = isLetter c || c == '_'
identCont c = isAlphaNum c || c `elem` ("_'-" :: String)
pathChar c = isAlphaNum c || c `elem` ("._/-+" :: String)
