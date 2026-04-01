{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | `tnix.config.tnix` loading and project scaffolding helpers.
--
-- The config format intentionally stays tiny and executable-free: it is a root
-- attribute set parsed with the ordinary tnix frontend, then decoded into a
-- concrete scaffold plan.
module Project
  ( initProject,
    scaffoldProject,
  )
where

import Control.Monad (forM)
import Data.List (group, partition, sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Parser (parseProgram)
import Syntax
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), isAbsolute, makeRelative, takeBaseName, takeDirectory)

data ProjectConfig = ProjectConfig
  { configRoot :: FilePath,
    configName :: Text,
    configSourceDir :: FilePath,
    configEntry :: FilePath,
    configDeclarationDir :: FilePath,
    configBuiltins :: Bool
  }

data PlannedFile = PlannedFile
  { plannedPath :: FilePath,
    plannedContent :: Text
  }

initProject :: Maybe FilePath -> IO (Either String Text)
initProject target = do
  root <- resolveRoot target
  createDirectoryIfMissing True root
  let configPath = root </> configFileName
  configExists <- doesFileExist configPath
  if configExists
    then pure (Left ("tnix.config.tnix already exists in " <> root))
    else do
      let config = defaultConfig root
      TextIO.writeFile configPath (renderConfig config)
      scaffoldResult <- scaffoldFromConfig config
      pure $
        fmap
          (\summary -> Text.unlines ("created: " : ("- " <> Text.pack configPath) : Text.lines summary))
          scaffoldResult

scaffoldProject :: Maybe FilePath -> IO (Either String Text)
scaffoldProject target = do
  root <- resolveRoot target
  loadProjectConfig (root </> configFileName) >>= either (pure . Left) scaffoldFromConfig

configFileName :: FilePath
configFileName = "tnix.config.tnix"

resolveRoot :: Maybe FilePath -> IO FilePath
resolveRoot = maybe getCurrentDirectory pure

defaultConfig :: FilePath -> ProjectConfig
defaultConfig root =
  let name = Text.pack (takeBaseName root)
      sourceDir = root </> "src"
      entry = sourceDir </> "main.tnix"
      declarationDir = root </> "types"
   in ProjectConfig
        { configRoot = root,
          configName = if Text.null name then "tnix-app" else name,
          configSourceDir = sourceDir,
          configEntry = entry,
          configDeclarationDir = declarationDir,
          configBuiltins = True
        }

renderConfig :: ProjectConfig -> Text
renderConfig config =
  Text.unlines
    [ "{",
      "  name = " <> quoted (configName config) <> ";",
      "  sourceDir = " <> prettyPath (configSourceDir config) <> ";",
      "  entry = " <> prettyPath (configEntry config) <> ";",
      "  declarationDir = " <> prettyPath (configDeclarationDir config) <> ";",
      "  builtins = " <> boolLiteral (configBuiltins config) <> ";",
      "}"
    ]
  where
    prettyPath path = Text.pack (relativizeFromRoot (configRoot config) path)

scaffoldFromConfig :: ProjectConfig -> IO (Either String Text)
scaffoldFromConfig config = do
  results <- traverse materializeFile (plannedFiles config)
  let (created, skipped) = partition fst results
      renderEntry (createdFile, path) =
        (if createdFile then "- created " else "- skipped ") <> Text.pack path
  pure $
    Right $
      Text.unlines $
        ["scaffolded " <> configName config]
          <> map renderEntry created
          <> map renderEntry skipped

plannedFiles :: ProjectConfig -> [PlannedFile]
plannedFiles config =
  [ PlannedFile (configRoot config </> "tnix.config.d.tnix") (renderConfigDeclarationFile config),
    PlannedFile (configEntry config) (renderEntryFile config)
  ]
    <> [PlannedFile (configDeclarationDir config </> "builtins.d.tnix") builtinsTemplate | configBuiltins config]

materializeFile :: PlannedFile -> IO (Bool, FilePath)
materializeFile planned = do
  exists <- doesFileExist (plannedPath planned)
  if exists
    then pure (False, plannedPath planned)
    else do
      createDirectoryIfMissing True (takeDirectory (plannedPath planned))
      TextIO.writeFile (plannedPath planned) (plannedContent planned)
      pure (True, plannedPath planned)

loadProjectConfig :: FilePath -> IO (Either String ProjectConfig)
loadProjectConfig configPath = do
  exists <- doesFileExist configPath
  if not exists
    then pure (Left ("missing tnix.config.tnix in " <> takeDirectory configPath))
    else do
      input <- TextIO.readFile configPath
      pure $ do
        program <- firstTextError ("failed to parse " <> configPath <> ": ") (parseProgram configPath input)
        expr <- maybe (Left "tnix.config.tnix must contain a root attribute set") (Right . markedValue) (programExpr program)
        fields <- decodeAttrSet expr
        let root = takeDirectory configPath
            fallbackName = Text.pack (takeBaseName root)
        name <- maybe (Right fallbackName) decodeStringField (Map.lookup "name" fields)
        sourceDir <- maybe (Right (root </> "src")) (decodePathField root "sourceDir") (Map.lookup "sourceDir" fields)
        entry <- maybe (Right (sourceDir </> "main.tnix")) (decodePathField root "entry") (Map.lookup "entry" fields)
        declarationDir <- maybe (Right (root </> "types")) (decodePathField root "declarationDir") (Map.lookup "declarationDir" fields)
        builtins <- maybe (Right True) (decodeBoolField "builtins") (Map.lookup "builtins" fields)
        pure
          ProjectConfig
            { configRoot = root,
              configName = if Text.null name then "tnix-app" else name,
              configSourceDir = sourceDir,
              configEntry = entry,
              configDeclarationDir = declarationDir,
              configBuiltins = builtins
            }

decodeAttrSet :: Expr -> Either String (Map.Map Text Expr)
decodeAttrSet = \case
  EAttrSet items ->
    let fields = [name | AttrField name _ <- items]
        duplicates = duplicateNames fields
     in case duplicates of
          duplicate : _ -> Left ("duplicate config field: " <> Text.unpack duplicate)
          [] ->
            fmap Map.fromList $
              forM items $ \case
                AttrField name expr -> pure (name, expr)
                AttrInherit _ -> Left "tnix.config.tnix does not support inherit in the root attrset"
  _ -> Left "tnix.config.tnix must evaluate to an attrset"

decodeStringField :: Expr -> Either String Text
decodeStringField = \case
  EString text -> Right text
  other -> Left ("expected string field in tnix.config.tnix, but got " <> show other)

decodeBoolField :: Text -> Expr -> Either String Bool
decodeBoolField name = \case
  EBool value -> Right value
  other -> Left ("expected Bool for " <> Text.unpack name <> ", but got " <> show other)

decodePathField :: FilePath -> Text -> Expr -> Either String FilePath
decodePathField root name = \case
  EPath path -> Right (resolveConfigPath root path)
  EString text -> Right (resolveConfigPath root (Text.unpack text))
  other -> Left ("expected path-like field for " <> Text.unpack name <> ", but got " <> show other)

resolveConfigPath :: FilePath -> FilePath -> FilePath
resolveConfigPath root path
  | isAbsolute path = path
  | otherwise = root </> dropDotSlash path

dropDotSlash :: FilePath -> FilePath
dropDotSlash path =
  case path of
    '.' : '/' : rest -> rest
    _ -> path

relativizeFromRoot :: FilePath -> FilePath -> FilePath
relativizeFromRoot root path = "./" <> makeRelative root path

quoted :: Text -> Text
quoted text = "\"" <> text <> "\""

boolLiteral :: Bool -> Text
boolLiteral True = "true"
boolLiteral False = "false"

renderEntryFile :: ProjectConfig -> Text
renderEntryFile config =
  Text.unlines
    [ "let",
      "  greeting :: String;",
      "  greeting = " <> quoted ("Hello from " <> configName config) <> ";",
      "in greeting"
    ]

renderConfigDeclarationFile :: ProjectConfig -> Text
renderConfigDeclarationFile _ =
  Text.unlines
    [ "type TnixProjectPath = Path | String;",
      "",
      "type TnixProjectConfig = {",
      "  name :: String;",
      "  sourceDir :: TnixProjectPath;",
      "  entry :: TnixProjectPath;",
      "  declarationDir :: TnixProjectPath;",
      "  builtins :: Bool;",
      "};",
      "",
      "declare " <> quoted "./tnix.config.tnix" <> " {",
      "  default :: TnixProjectConfig;",
      "};"
    ]

builtinsTemplate :: Text
builtinsTemplate =
  Text.unlines
    [ "declare \"builtins\" {",
      "  head :: forall a. List a -> a;",
      "  import :: Path -> dynamic;",
      "  length :: forall a. List a -> Int;",
      "  map :: forall a b. (a -> b) -> List a -> List b;",
      "};"
    ]

firstTextError :: String -> Either Text a -> Either String a
firstTextError prefix = either (Left . (prefix <>) . Text.unpack) Right

duplicateNames :: Ord a => [a] -> [a]
duplicateNames = foldr step [] . group . sort
  where
    step values acc =
      case values of
        first : _ | length values > 1 -> first : acc
        _ -> acc
