{-# LANGUAGE OverloadedStrings #-}

-- | Testable JSON-RPC/LSP helpers for tnix.
module Server
  ( asInt,
    asText,
    clearDiagnostics,
    contentLengthFromHeaders,
    diag,
    documentPath,
    field,
    firstChange,
    hoverResult,
    notify,
    pathUri,
    publishDiagnostics,
    readMessage,
    respond,
    send,
    uriPath,
    wordAt,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Driver (Analysis (..), lookupSymbolType)
import Pretty (renderScheme)
import System.IO (Handle, hIsEOF)

field :: Text -> Value -> Maybe Value
field key (Object obj) = KeyMap.lookup (Key.fromText key) obj
field _ _ = Nothing

asText :: Value -> Maybe Text
asText (String text) = Just text
asText _ = Nothing

asInt :: Value -> Int
asInt (Number n) = floor n
asInt _ = 0

firstChange :: Maybe Value -> Maybe Value
firstChange params = do
  Array changes <- field "contentChanges" =<< params
  listToMaybe (toList changes)

diag :: String -> Value
diag err = object ["range" .= object ["start" .= pos, "end" .= pos], "severity" .= (1 :: Int), "message" .= err]
  where
    pos = object ["line" .= (0 :: Int), "character" .= (0 :: Int)]

publishDiagnostics :: FilePath -> Either String Analysis -> Value
publishDiagnostics file result =
  object
    [ "uri" .= pathUri file,
      "diagnostics"
        .= either
          (\err -> [diag err])
          (const ([] :: [Value]))
          result
    ]

hoverResult :: Either String Analysis -> Text -> Int -> Int -> Value
hoverResult result content lineNo charNo =
  object
    [ "contents"
        .= object
          [ "kind" .= ("markdown" :: Text),
            "value" .= ("```tnix\n" <> rendered <> "\n```" :: Text)
          ]
    ]
  where
    rendered =
      case result of
        Left err -> T.pack err
        Right analysis ->
          let symbol = wordAt lineNo charNo content
           in maybe (maybe "No type information." renderScheme (analysisRoot analysis)) renderScheme (lookupSymbolType analysis symbol)

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

documentPath :: Maybe Value -> Maybe FilePath
documentPath params = do
  textDocument <- field "textDocument" =<< params
  uri <- field "uri" textDocument >>= asText
  pure (uriPath uri)

clearDiagnostics :: FilePath -> Value
clearDiagnostics file = object ["uri" .= pathUri file, "diagnostics" .= ([] :: [Value])]

contentLengthFromHeaders :: [BS.ByteString] -> Maybe Int
contentLengthFromHeaders headers =
  listToMaybe $
    mapMaybe parseHeader headers
  where
    parseHeader header = do
      value <- stripPrefixCI "content-length:" (B8.unpack header)
      case reads (dropWhile (== ' ') value) of
        [(len, "")] -> Just len
        _ -> Nothing
    stripPrefixCI needle haystack =
      stripPrefix (map toLower needle) (map toLower haystack)

readMessage :: Handle -> IO (Maybe Value)
readMessage handle = do
  eof <- hIsEOF handle
  if eof
    then pure Nothing
    else do
      headers <- readHeaders handle
      case contentLengthFromHeaders headers of
        Nothing -> pure Nothing
        Just len -> decodeStrict' <$> BS.hGet handle len

send :: Handle -> Value -> IO ()
send handle payload = do
  let body = encode payload
  B8.hPutStr handle ("Content-Length: " <> B8.pack (show (LBS.length body)) <> "\r\n\r\n")
  LBS.hPutStr handle body

respond :: Handle -> Value -> Value -> IO ()
respond handle request result =
  case field "id" request of
    Just ident -> send handle (object ["jsonrpc" .= ("2.0" :: Text), "id" .= ident, "result" .= result])
    Nothing -> pure ()

notify :: Handle -> Text -> Value -> IO ()
notify handle method params = send handle (object ["jsonrpc" .= ("2.0" :: Text), "method" .= method, "params" .= params])

readHeaders :: Handle -> IO [BS.ByteString]
readHeaders handle = go []
  where
    go acc = do
      line <- B8.hGetLine handle
      let trimmed = B8.filter (/= '\r') line
      if BS.null trimmed
        then pure (reverse acc)
        else go (trimmed : acc)
