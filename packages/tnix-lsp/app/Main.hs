{-# LANGUAGE OverloadedStrings #-}

-- | Minimal JSON-RPC/LSP bridge for tnix.
--
-- This server intentionally implements only the core interactions needed to
-- prove the architecture: opening/changing documents, diagnostics, and hover.
-- It delegates all semantic work to 'Tnix.Driver'.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (forever, void)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (exitSuccess)
import System.IO (stdin, stdout)
import Tnix.Driver
import Tnix.Pretty (renderScheme)

-- | Start the stdio event loop and keep the latest document text in memory.
main :: IO ()
main = do
  ref <- newIORef Map.empty
  forever $ readMessage >>= maybe (pure ()) (handle ref)

-- | Dispatch one incoming JSON-RPC message.
handle :: IORef (Map.Map FilePath Text) -> Value -> IO ()
handle ref msg = case field "method" msg >>= asText of
  Just "initialize" -> respond msg (object ["capabilities" .= object ["hoverProvider" .= True, "textDocumentSync" .= (1 :: Int)]])
  Just "shutdown" -> respond msg Null
  Just "exit" -> exitSuccess
  Just "textDocument/didOpen" -> update ref msg >>= publish
  Just "textDocument/didChange" -> update ref msg >>= publish
  Just "textDocument/hover" -> hover ref msg >>= respond msg
  _ -> pure ()

-- | Update the in-memory copy of a document and re-run analysis.
update :: IORef (Map.Map FilePath Text) -> Value -> IO (FilePath, Either String Analysis)
update ref msg = do
  let params = field "params" msg
      textDocument = params >>= field "textDocument"
      file = maybe "" uriPath (textDocument >>= field "uri" >>= asText)
      text' = ((textDocument <|> firstChange params) >>= field "text") >>= asText
  content <- maybe (TIO.readFile file) pure text'
  modifyIORef' ref (Map.insert file content)
  result <- analyzeText file content
  pure (file, result)

-- | Extract the first full-text document change from an LSP change payload.
firstChange :: Maybe Value -> Maybe Value
firstChange params = do
  Array changes <- field "contentChanges" =<< params
  case toList changes of
    x : _ -> pure x
    _ -> Nothing

-- | Publish diagnostics for the latest analysis result.
publish :: (FilePath, Either String Analysis) -> IO ()
publish (file, result) =
  notify
    "textDocument/publishDiagnostics"
    ( object
        [ "uri" .= pathUri file,
          "diagnostics"
            .= either
              (\err -> [diag err])
              (const ([] :: [Value]))
              result
        ]
    )

-- | Compute hover contents at the requested position.
hover :: IORef (Map.Map FilePath Text) -> Value -> IO Value
hover ref msg = do
  let params = field "params" msg
      textDocument = params >>= field "textDocument"
      position = params >>= field "position"
      file = maybe "" uriPath (textDocument >>= field "uri" >>= asText)
      lineNo = maybe 0 asInt (position >>= field "line")
      charNo = maybe 0 asInt (position >>= field "character")
  docs <- readIORef ref
  content <- maybe (TIO.readFile file) pure (Map.lookup file docs)
  result <- analyzeText file content
  let value =
        case result of
          Left err -> T.pack err
          Right analysis ->
            let symbol = wordAt lineNo charNo content
             in maybe (maybe "No type information." renderScheme (analysisRoot analysis)) renderScheme (lookupSymbolType analysis symbol)
  pure (object ["contents" .= object ["kind" .= ("markdown" :: Text), "value" .= ("```tnix\n" <> value <> "\n```" :: Text)]])

field :: Text -> Value -> Maybe Value
field key (Object obj) = KeyMap.lookup (Key.fromText key) obj
field _ _ = Nothing

asText :: Value -> Maybe Text
asText (String text) = Just text
asText _ = Nothing

asInt :: Value -> Int
asInt (Number n) = floor n
asInt _ = 0

diag :: String -> Value
diag err = object ["range" .= object ["start" .= pos, "end" .= pos], "severity" .= (1 :: Int), "message" .= err]
  where
    pos = object ["line" .= (0 :: Int), "character" .= (0 :: Int)]

wordAt :: Int -> Int -> Text -> Text
wordAt lineNo charNo content =
  case drop lineNo (T.lines content) of
    line : _ -> let (a, b) = T.splitAt charNo line in takeWordEnd a <> takeWordStart b
    _ -> "default"
  where
    ok c = c == '_' || c == '-' || c == '\'' || c == '.' || c == '?' || c == '!' || c `elem` ['0' .. '9'] || c `elem` ['A' .. 'Z'] || c `elem` ['a' .. 'z']
    takeWordEnd = T.reverse . T.takeWhile ok . T.reverse
    takeWordStart = T.takeWhile ok

pathUri :: FilePath -> Text
pathUri file = "file://" <> T.pack file

uriPath :: Text -> FilePath
uriPath text = T.unpack (fromMaybe text (T.stripPrefix "file://" text))

respond :: Value -> Value -> IO ()
respond request result =
  case field "id" request of
    Just ident -> send (object ["jsonrpc" .= ("2.0" :: Text), "id" .= ident, "result" .= result])
    Nothing -> pure ()

notify :: Text -> Value -> IO ()
notify method params = send (object ["jsonrpc" .= ("2.0" :: Text), "method" .= method, "params" .= params])

send :: Value -> IO ()
send payload = do
  let body = encode payload
  B8.hPutStr stdout ("Content-Length: " <> B8.pack (show (LBS.length body)) <> "\r\n\r\n")
  LBS.hPutStr stdout body

readMessage :: IO (Maybe Value)
readMessage = do
  header <- B8.hGetLine stdin
  if BS.null header
    then pure Nothing
    else do
      let len = read (B8.unpack (B8.dropWhile (not . (`elem` ['0' .. '9'])) header)) :: Int
      void (B8.hGetLine stdin)
      decodeStrict' <$> BS.hGet stdin len
