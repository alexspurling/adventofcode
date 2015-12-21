module Spec (main, spec) where

import Test.Hspec

day1 :: Spec
day1 =
  describe "Day1" $ do
      context "stepsToBasement" $ do
        it "returns 1 for 1" $ do 
          "hello" `shouldBe` "hello"

spec :: IO Spec
spec = do
  str <- getLine
  return day1
    

main :: IO ()
main = putStrLn "Running"