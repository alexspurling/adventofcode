module Day13Spec where

import Test.Hspec
import Day13

spec :: Spec
spec =
  describe "Day13" $ do
    it "Maximum guest happiness is 664" $ do
      Day13.maximumGuestHappiness `shouldBe` 664

specIO :: IO Spec
specIO = return spec