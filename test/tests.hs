module Main where

import Discounts
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Discounts" $ do
    it "has a discount that is 0" $ do
      discount `shouldBe` 0
