{-# LANGUAGE OverloadedStrings #-}

-- | Top-level parsing entry point.
--
-- This wrapper converts Megaparsec's rich diagnostic bundle into plain text so
-- the rest of the pipeline can forward parse errors through the CLI, LSP, and
-- tests without depending on parser-specific types.
module Tnix.Parser (parseProgram) where

import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec (eof, errorBundlePretty, runParser)
import Tnix.Parser.Expr
import Tnix.Parser.Lexer
import Tnix.Syntax

-- | Parse a complete tnix program.
parseProgram :: FilePath -> Text -> Either Text Program
parseProgram path input =
  case runParser (sc *> programParser <* eof) path input of
    Left err -> Left (Text.pack (errorBundlePretty err))
    Right program -> Right program
