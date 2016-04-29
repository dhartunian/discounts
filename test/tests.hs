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
productDb = Data.Map.fromList [ (1, Product (pack "Black Jacobins") 2000) , (2, Product (pack "Shock Doctrine") 1500)]


main :: IO ()
main = hspec $ do
  describe "Discounts" $ do
    describe "itemCost" $ do
      it "multiplies quantity of item with its cost to determine the total item cost" $ do
        itemCost productDb (LineItem 1 2) `shouldBe` Just 4000
      it "has an undefined cost if product does not exist" $ do
        itemCost productDb (LineItem 3 2) `shouldBe` Nothing
    describe "discountedItemCost" $ do
      it "applies the discount as a percentage to the item cost rounded down" $ do
        discountedItemCost productDb (Discount 50 All Nothing) (LineItem 2 1) `shouldBe` Just 750
      it "does not apply the discount if we have too many and the discount is limited" $ do
        discountedItemCost productDb (Discount 50 All (Just 1)) (LineItem 2 2) `shouldBe` Just 3000
      it "only applies a discount limited to product list to those products only" $ do
        discountedItemCost productDb (Discount 50 (Some [1]) Nothing) (LineItem 1 1) `shouldBe` Just 1000
        discountedItemCost productDb (Discount 50 (Some [1]) Nothing) (LineItem 2 1) `shouldBe` Just 1500
    describe "applyDiscount" $ do
      it "multiplies cost by percentage discount rounded down" $ do
        applyDiscount 100 50 `shouldBe` 50
        applyDiscount 5 50 `shouldBe` 2
        applyDiscount 1 50 `shouldBe` 0
        applyDiscount 75 22 `shouldBe` 16
    describe "appliesToQuantity" $ do
      it "returns true when the discount applies to all items and has no limit" $ do
        appliesToQuantity (LineItem 10 10000) (Discount 22 All Nothing) `shouldBe` True
      it "returns false when the discount is limited to n items and we have more" $ do
        appliesToQuantity (LineItem 10 11) (Discount 22 All (Just 10)) `shouldBe` False
      it "returns true when the discount is limited to n items and we have fewer or equal" $ do
        appliesToQuantity (LineItem 10 10) (Discount 22 All (Just 10)) `shouldBe` True
        appliesToQuantity (LineItem 10 8) (Discount 22 All (Just 10)) `shouldBe` True
    describe "appliesToProduct" $ do
      it "returns true if discount is for All" $ do
        appliesToProduct (LineItem 23 10) (Discount 12 All (Just 10)) `shouldBe` True
      it "returns true if discount is for product in list" $ do
        appliesToProduct (LineItem 23 10) (Discount 12 (Some [1..100]) Nothing) `shouldBe` True
        appliesToProduct (LineItem 1 10) (Discount 12 (Some [1..100]) Nothing) `shouldBe` True
        appliesToProduct (LineItem 100 10) (Discount 12 (Some [1..100]) Nothing) `shouldBe` True
      it "returns false if product is not in list" $ do
        appliesToProduct (LineItem 101 10) (Discount 12 (Some [1..100]) Nothing) `shouldBe` False
        appliesToProduct (LineItem 140 10) (Discount 12 (Some [1..100]) Nothing) `shouldBe` False
    describe "orderCost" $ do
      it "adds up the cost of all line items in the order" $ do
        orderCost productDb discountDb (Order [(LineItem 1 1), (LineItem 2 1)] Nothing) `shouldBe` 1500 + 2000
      it "adds up the cost of all line items in the order" $ do
        orderCost productDb discountDb (Order [(LineItem 1 2), (LineItem 2 5)] Nothing) `shouldBe` (2 * 2000) + (5 * 1500)
      it "applies a discount if one exists and applies to item" $ do
        orderCost productDb discountDb sampleOrder2 `shouldBe` 1000
        orderCost productDb discountDb (Order [LineItem 1 1] (Just 2)) `shouldBe` 1500
        orderCost productDb discountDb (Order [LineItem 2 2] (Just 1)) `shouldBe` 3000
        orderCost productDb discountDb (Order [LineItem 2 2] (Just 2)) `shouldBe` 3000
