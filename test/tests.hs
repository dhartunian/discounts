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
-- discount 1 applies to all products but we only have 1 per order
-- discount 2 applies to only product 1 but is unlimited
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
        itemCost productDb (LineItem 1 2) `shouldBe` Right 4000
      it "has an undefined cost if product does not exist" $ do
        itemCost productDb (LineItem 3 2) `shouldBe` Left ProductDoesNotExist
    describe "discountedItemCost" $ do
      it "applies the discount as a percentage to the item cost rounded down" $ do
        discountedItemCost productDb (Discount 50 All Nothing) (LineItem 2 1) `shouldBe` Right 750
      it "does not apply the discount if we have too many and the discount is limited" $ do
        discountedItemCost productDb (Discount 50 All (Just 1)) (LineItem 2 2) `shouldBe` Right 3000
      it "only applies a discount limited to product list to those products only" $ do
        discountedItemCost productDb (Discount 50 (Some [1]) Nothing) (LineItem 1 1) `shouldBe` Right 1000
        discountedItemCost productDb (Discount 50 (Some [1]) Nothing) (LineItem 2 1) `shouldBe` Right 1500
