module Day7Spec (specIO) where

import Test.Hspec
import Day7

spec :: String -> Spec
spec input =
  describe "Day7" $ do
    it "Identity of 123 should be 123" $ do
      Day7.evaluateOperator (Identity (ValueOperand 123)) `shouldBe` 123
    it "NOT 123 should be 65412" $ do
      Day7.evaluateOperator (Not (ValueOperand 123)) `shouldBe` 65412
    it "123 AND 456 should be 72" $ do
      Day7.evaluateOperator (And (ValueOperand 123) (ValueOperand 456)) `shouldBe` 72
    it "123 OR 456 should be 507" $ do
      Day7.evaluateOperator (Or (ValueOperand 123) (ValueOperand 456)) `shouldBe` 507
    it "123 LShift 2 should be 492" $ do
      Day7.evaluateOperator (Lshift (ValueOperand 123) (ValueOperand 2)) `shouldBe` 492
    it "123 RShift 2 should be 30" $ do
      Day7.evaluateOperator (Rshift (ValueOperand 123) (ValueOperand 2)) `shouldBe` 30
    it "Value of signal `a` is 1234." $ do
      Day7.valueOfA input `shouldBe` "foo"

specIO :: IO Spec
specIO = do
  input <- readFile "test/Day7.txt"
  return (spec input)