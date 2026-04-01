{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Driver (Analysis (..), analyzeFile)
import Indexed (validateProgramIndexedTypes)
import Kind (validateProgramKinds)
import Parser (parseProgram)
import Pretty (renderScheme)
import Syntax (Program (programAliases, programExpr))
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import Test.Hspec
import TestSupport (expectRight, source, withTempTree)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ecosystem registry" $ do
  it "parses and validates the bundled registry declaration files" $ do
    root <- findRepoRoot =<< getCurrentDirectory
    forM_ registryFiles $ \relative -> do
      input <- TextIO.readFile (root </> relative)
      program <- expectRight (parseProgram (root </> relative) input)
      programExpr program `shouldBe` Nothing
      validateProgramKinds (programAliases program) program `shouldSatisfy` isRight
      validateProgramIndexedTypes program `shouldBe` Right ()

  it "lets projects reuse bundled nixpkgs aliases inside ambient declarations" $ do
    root <- findRepoRoot =<< getCurrentDirectory
    registry <- loadRegistry root ["registry/nixpkgs-lib.d.tnix", "registry/nixpkgs-pkgs.d.tnix"]
    withTempTree
      ( registry
          <> [ ( "types.d.tnix",
                 source
                   [ "declare \"./lib.nix\" { default :: NixpkgsLib; };",
                     "declare \"./pkgs.nix\" { default :: NixpkgsPkgs; };"
                   ]
               ),
               ("lib-surface.tnix", "let lib = import ./lib.nix; in lib.strings.concatStringsSep"),
               ("pkgs-surface.tnix", "let pkgs = import ./pkgs.nix; in pkgs.writeShellScriptBin")
             ]
      )
      ( \tmp -> do
          libAnalysis <- analyzeFile (tmp </> "lib-surface.tnix") >>= expectRight
          pkgsAnalysis <- analyzeFile (tmp </> "pkgs-surface.tnix") >>= expectRight
          renderedRoot libAnalysis `shouldBe` "String -> List String -> String"
          let renderedPkgs = renderedRoot pkgsAnalysis
          Text.isInfixOf "String -> String ->" renderedPkgs `shouldBe` True
          Text.isInfixOf "derivationType :: \"derivation\"" renderedPkgs `shouldBe` True
      )

  it "lets projects reuse bundled flake ecosystem aliases" $ do
    root <- findRepoRoot =<< getCurrentDirectory
    registry <- loadRegistry root registryFiles
    withTempTree
      ( registry
          <> [ ( "types.d.tnix",
                 source
                   [ "declare \"./flake-utils.nix\" { default :: NixFlakeUtilsFlake; };",
                     "declare \"./nixpkgs-flake.nix\" { default :: NixpkgsFlake; };",
                     "declare \"./home-manager.nix\" { default :: HomeManagerFlake; };",
                     "declare \"./nix-darwin.nix\" { default :: NixDarwinFlake; };",
                     "declare \"./flake-parts.nix\" { default :: FlakePartsLib; };"
                   ]
               ),
               ("flake-utils-surface.tnix", "let value = import ./flake-utils.nix; in value.lib.eachDefaultSystem"),
               ("nixpkgs-flake-surface.tnix", "let value = import ./nixpkgs-flake.nix; in value.lib.optional"),
               ("home-manager-surface.tnix", "let value = import ./home-manager.nix; in value.lib.homeManagerConfiguration"),
               ("nix-darwin-surface.tnix", "let value = import ./nix-darwin.nix; in value.lib.darwinSystem"),
               ("flake-parts-surface.tnix", "let value = import ./flake-parts.nix; in value.mkFlake")
             ]
      )
      ( \tmp -> do
          flakeUtilsAnalysis <- analyzeFile (tmp </> "flake-utils-surface.tnix") >>= expectRight
          nixpkgsFlakeAnalysis <- analyzeFile (tmp </> "nixpkgs-flake-surface.tnix") >>= expectRight
          homeManagerAnalysis <- analyzeFile (tmp </> "home-manager-surface.tnix") >>= expectRight
          nixDarwinAnalysis <- analyzeFile (tmp </> "nix-darwin-surface.tnix") >>= expectRight
          flakePartsAnalysis <- analyzeFile (tmp </> "flake-parts-surface.tnix") >>= expectRight
          let renderedFlakeUtils = renderedRoot flakeUtilsAnalysis
              renderedNixpkgsFlake = renderedRoot nixpkgsFlakeAnalysis
              renderedNixDarwin = renderedRoot nixDarwinAnalysis
          Text.isInfixOf "forall" renderedFlakeUtils `shouldBe` True
          Text.isInfixOf "\"aarch64-linux\"" renderedFlakeUtils `shouldBe` True
          Text.isInfixOf "x86_64-darwin" renderedFlakeUtils `shouldBe` True
          Text.isInfixOf "forall" renderedNixpkgsFlake `shouldBe` True
          Text.isInfixOf "Bool ->" renderedNixpkgsFlake `shouldBe` True
          Text.isInfixOf "List" renderedNixpkgsFlake `shouldBe` True
          renderedRoot homeManagerAnalysis `shouldBe` "dynamic -> dynamic"
          Text.isInfixOf "modules :: List dynamic" renderedNixDarwin `shouldBe` True
          Text.isInfixOf "pkgs ::" renderedNixDarwin `shouldBe` True
          Text.isInfixOf "system ::" renderedNixDarwin `shouldBe` True
          Text.isSuffixOf "} -> dynamic" renderedNixDarwin `shouldBe` True
          renderedRoot flakePartsAnalysis `shouldBe` "dynamic -> dynamic -> dynamic"
      )

registryFiles :: [FilePath]
registryFiles =
  [ "registry/nixpkgs-lib.d.tnix",
    "registry/nixpkgs-pkgs.d.tnix",
    "registry/flake-ecosystem.d.tnix"
  ]

loadRegistry :: FilePath -> [FilePath] -> IO [(FilePath, Text)]
loadRegistry root files =
  forM files $ \relative -> do
    content <- TextIO.readFile (root </> relative)
    pure (relative, content)

findRepoRoot :: FilePath -> IO FilePath
findRepoRoot start = go start
  where
    go dir = do
      marker <- doesFileExist (dir </> "cabal.project")
      let parent = takeDirectory dir
      if marker
        then pure dir
        else
          if parent == dir
            then expectationFailure ("could not find cabal.project above " <> start) >> fail "missing repo root"
            else go parent

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

renderedRoot :: Analysis -> Text
renderedRoot = maybe "" renderScheme . analysisRoot
