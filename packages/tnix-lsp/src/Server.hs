{-# LANGUAGE OverloadedStrings #-}

-- | Testable JSON-RPC/LSP helpers for tnix.
module Server
  ( applyContentChanges,
    asInt,
    asText,
    clearDiagnostics,
    clientCapabilities,
    contentLengthFromHeaders,
    contentChanges,
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
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, toLower, toUpper)
import Data.Foldable (toList)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEncoding
import Driver (Analysis (..), lookupSymbolType)
import Numeric (showHex)
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

clientCapabilities :: Value
clientCapabilities =
  object
    [ "capabilities"
        .= object
          [ "hoverProvider" .= True,
            "textDocumentSync" .= object ["openClose" .= True, "change" .= (2 :: Int)]
          ]
    ]

firstChange :: Maybe Value -> Maybe Value
firstChange params = do
  Array changes <- field "contentChanges" =<< params
  listToMaybe (toList changes)

contentChanges :: Maybe Value -> [Value]
contentChanges params =
  case field "contentChanges" =<< params of
    Just (Array changes) -> toList changes
    _ -> []

applyContentChanges :: Text -> Maybe Value -> Either String Text
applyContentChanges initial params =
  foldl' (\acc change -> acc >>= (`applyContentChange` change)) (Right initial) (contentChanges params)

applyContentChange :: Text -> Value -> Either String Text
applyContentChange content change = do
  replacement <- maybe (Left "content change is missing text") Right (field "text" change >>= asText)
  case field "range" change of
    Nothing -> Right replacement
    Just range -> do
      start <- rangePos "start" range >>= positionOffset content
      end <- rangePos "end" range >>= positionOffset content
      if start > end
        then Left "content change range is inverted"
        else Right (T.take start content <> replacement <> T.drop end content)
  where
    rangePos :: Text -> Value -> Either String (Int, Int)
    rangePos key range = do
      pos <- maybe (Left ("content change range is missing " <> T.unpack key)) Right (field key range)
      lineNo <- maybe (Left ("content change range is missing " <> T.unpack key <> ".line")) Right (field "line" pos)
      charNo <- maybe (Left ("content change range is missing " <> T.unpack key <> ".character")) Right (field "character" pos)
      Right (asInt lineNo, asInt charNo)

positionOffset :: Text -> (Int, Int) -> Either String Int
positionOffset content (lineNo, charNo) =
  go 0 lineNo (T.splitOn "\n" content)
  where
    go offset 0 (line : _) =
      if charNo <= T.length line
        then Right (offset + charNo)
        else Left "content change character is out of bounds"
    go offset n (line : rest) =
      go (offset + T.length line + 1) (n - 1) rest
    go _ _ [] = Left "content change line is out of bounds"

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
pathUri file = "file://" <> percentEncode (T.pack file)

uriPath :: Text -> FilePath
uriPath text = T.unpack (percentDecode (stripAuthority (fromMaybe text (T.stripPrefix "file://" text))))

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

percentEncode :: Text -> Text
percentEncode =
  T.concatMap encodeChar
  where
    encodeChar char
      | isUnreserved char || char == '/' = T.singleton char
      | otherwise = T.concat (map encodeByte (BS.unpack (TextEncoding.encodeUtf8 (T.singleton char))))
    encodeByte byte =
      let hex = showHex byte ""
       in T.pack ['%', toUpper (pad hex !! 0), toUpper (pad hex !! 1)]
    pad [digit] = ['0', digit]
    pad digits = digits
    isUnreserved char = isAsciiLower char || isAsciiUpper char || isDigit char || char `elem` ['-', '.', '_', '~']

percentDecode :: Text -> Text
percentDecode text =
  case TextEncoding.decodeUtf8' (BS.pack (decodeBytes (T.unpack text))) of
    Left _ -> text
    Right decoded -> decoded
  where
    decodeBytes ('%' : a : b : rest)
      | isHexDigit a && isHexDigit b = fromIntegral (digitToInt a * 16 + digitToInt b) : decodeBytes rest
    decodeBytes (char : rest) = BS.unpack (TextEncoding.encodeUtf8 (T.singleton char)) <> decodeBytes rest
    decodeBytes [] = []

stripAuthority :: Text -> Text
stripAuthority path =
  fromMaybe path (T.stripPrefix "localhost" path)
