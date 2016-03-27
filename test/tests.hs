module Main where

import Discounts
import Test.Hspec
import Data.Text
import Data.Map

sampleOrder1 :: Order
sampleOrder1 = Order [LineItem 1 1] Nothing

sampleOrder2 :: Order
sampleOrder2 = Order [LineItem 1 1] (Just 1)

-- added () around Some [1]
discountDb :: Map Int Discount
discountDb = Data.Map.fromList [ (1, Discount 50 All (Just 1))
                               , (2, Discount 75 (Some [1]) Nothing)]

-- wrapped strings in pack to turn into Text
productDb :: Map Int Product
productDb = Data.Map.fromList [ (1, Product (pack "Black Jacobins") 2000)
                              , (2, Product (pack "Shock Doctrine") 1500)]


main :: IO ()
main = hspec $ do
  describe "Discounts" $ do
    describe "itemCost" $ do
      it "multiplies quantity of item with its cost to determine the total item cost" $ do
        itemCost productDb (LineItem 1 2) `shouldBe` 4000
