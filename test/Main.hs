module Main where

import Test.Hspec
import qualified SidonSpec

main :: IO ()
main = hspec $ do
  SidonSpec.spec
