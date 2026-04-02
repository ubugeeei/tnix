{-# LANGUAGE OverloadedStrings #-}

module TestSupport
  ( expectLeftContaining,
    expectRight,
    fixturePath,
    source,
    withTempTree,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>), normalise, takeDirectory)
import System.IO (hClose, openTempFile)
import Test.Hspec (Expectation, expectationFailure)

expectRight :: Show e => Either e a -> IO a
expectRight (Right value) = pure value
expectRight (Left err) = expectationFailure ("expected Right, got Left: " <> show err) >> fail "expected Right"

expectLeftContaining :: Either String a -> String -> Expectation
expectLeftContaining result needle =
  case result of
    Left err | Text.pack needle `Text.isInfixOf` Text.pack err -> pure ()
    Left err -> expectationFailure ("expected error containing " <> show needle <> ", got " <> show err)
    Right _ -> expectationFailure ("expected Left containing " <> show needle <> ", got Right")

source :: [Text] -> Text
source = Text.unlines

fixturePath :: FilePath -> IO FilePath
fixturePath relative = do
  cwd <- getCurrentDirectory
  findRepoRoot (normalise cwd)
  where
    findRepoRoot dir = do
      let markerFile = dir </> "cabal.project"
          markerDir = dir </> ".git"
      hasProject <- doesFileExist markerFile
      hasGit <- doesDirectoryExist markerDir
      if hasProject || hasGit
        then pure (dir </> relative)
        else
          let parent = normalise (takeDirectory dir)
           in if parent == dir
                then expectationFailure ("could not locate repo root for fixture path " <> show relative) >> fail "missing repo root"
                else findRepoRoot parent

withTempTree :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withTempTree files action = bracket createRoot removePathForcibly (\root -> writeTree root >> action root)
  where
    createRoot = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp "tnix-test"
      hClose handle
      removeFile path
      createDirectory path
      pure path
    writeTree root =
      forM_ files $ \(relative, content) -> do
        let path = root </> relative
        createDirectoryIfMissing True (takeDirectory path)
        Text.writeFile path content
