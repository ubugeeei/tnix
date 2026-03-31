module Main (main) where

import CompileEmitSpec qualified
import DriverSpec qualified
import ParserSpec qualified
import SubtypingSpec qualified
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    ParserSpec.spec
    SubtypingSpec.spec
    DriverSpec.spec
    CompileEmitSpec.spec
