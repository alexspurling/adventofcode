module Day15Spec where

import Test.Hspec
import Day15

spec :: Spec
spec =
  describe "Day15" $ do
    it "Best score for cookie is 13882464" $ do
      Day15.bestCookieScore `shouldBe` 13882464
    it "Best score for cookie with 500 calories is 11171160" $ do
      Day15.bestHealthyCookieScore `shouldBe` 11171160

specIO :: IO Spec
specIO = return spec