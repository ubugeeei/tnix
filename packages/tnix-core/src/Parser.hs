{-# LANGUAGE OverloadedStrings #-}

-- | Top-level parsing entry point.
--
-- This wrapper converts Megaparsec's rich diagnostic bundle into plain text so
-- the rest of the pipeline can forward parse errors through the CLI, LSP, and
-- tests without depending on parser-specific types.
module Parser (parseProgram) where

import Control.Monad.Reader (runReaderT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map.Strict qualified as Map
import Text.Megaparsec (eof, errorBundlePretty, runParser)
import ParserExpr
import ParserLexer
import Syntax

-- | Parse a complete tnix program.
parseProgram :: FilePath -> Text -> Either Text Program
parseProgram path input = do
  directives <- scanDiagnosticDirectives input
  case runParser (runReaderT (sc *> programParser <* eof) directives) path input of
    Left err -> Left (Text.pack (errorBundlePretty err))
    Right program -> Right program

scanDiagnosticDirectives :: Text -> Either Text DirectiveTargets
scanDiagnosticDirectives input = go 1 Nothing Map.empty (Text.lines input)
  where
    go _ Nothing acc [] = Right acc
    go lineNo (Just _) _ [] = Left ("dangling tnix diagnostic directive before end of file at line " <> Text.pack (show (lineNo - 1)))
    go lineNo pending acc (line : rest) =
      case parseDirective line of
        Just directive ->
          case pending of
            Just _ -> Left ("multiple tnix diagnostic directives target the same next line before line " <> Text.pack (show lineNo))
            Nothing -> go (lineNo + 1) (Just directive) acc rest
        Nothing
          | isSkippableLine line -> go (lineNo + 1) pending acc rest
          | otherwise ->
              case pending of
                Nothing -> go (lineNo + 1) Nothing acc rest
                Just directive ->
                  case Map.lookup lineNo acc of
                    Just _ -> Left ("duplicate tnix diagnostic directives for line " <> Text.pack (show lineNo))
                    Nothing -> go (lineNo + 1) Nothing (Map.insert lineNo directive acc) rest

parseDirective :: Text -> Maybe DiagnosticDirective
parseDirective raw =
  case Text.stripStart raw of
    trimmed
      | Just rest <- Text.stripPrefix "#" trimmed ->
          let body = Text.stripStart rest
           in if "@tnix-ignore" `Text.isPrefixOf` body
                then Just TnixIgnore
                else
                  if "@tnix-expected" `Text.isPrefixOf` body
                    then Just TnixExpected
                    else Nothing
      | otherwise -> Nothing

isSkippableLine :: Text -> Bool
isSkippableLine raw =
  case Text.strip raw of
    "" -> True
    trimmed -> "#" `Text.isPrefixOf` trimmed
