{-# LANGUAGE OverloadedStrings #-}

module Tnix.Parser (parseProgram) where

import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec (eof, errorBundlePretty, runParser)
import Tnix.Parser.Expr
import Tnix.Parser.Lexer
import Tnix.Syntax

parseProgram :: FilePath -> Text -> Either Text Program
parseProgram path input =
  case runParser (sc *> programParser <* eof) path input of
    Left err -> Left (Text.pack (errorBundlePretty err))
    Right program -> Right program
