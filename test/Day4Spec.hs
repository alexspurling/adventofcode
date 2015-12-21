module Day4Spec (specIO) where

import Test.Hspec
import qualified Day4 as Day4

spec :: Spec
spec =
  describe "Day4" $ do
    it "First hash number is 254575" $ do
      Day4.getHashNumber `shouldBe` Just 254575
    it "Second hash number is 1038736" $ do
      Day4.getHashNumber2 `shouldBe` Just 1038736

specIO :: IO Spec
specIO = return spec