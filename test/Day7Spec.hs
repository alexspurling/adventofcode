module Day7Spec (specIO) where

import Test.Hspec
import qualified Day7 as Day7

spec :: String -> Spec
spec input =
  describe "Day7" $ do
    it "Value of signal `a` is 1234." $ do
      Day7.valueOfA input `shouldBe` "foo"

specIO :: IO Spec
specIO = do
  input <- readFile "test/Day7.txt"
  return (spec input)