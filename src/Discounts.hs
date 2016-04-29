module Discounts where

import Data.Text
import Data.Map
import Data.Maybe (catMaybes)

data LineItem = LineItem { lineItemProductId :: Int
                         , lineItemQuantity :: Int } deriving (Eq, Show)

data Order = Order { orderLineItems :: [LineItem]
                   , orderDiscountId :: Maybe Int } deriving (Eq, Show)

data Mode = All | Some [Int] deriving (Eq, Show)

-- changed type "Variant" of discountVariant to "Mode" since that's
-- what was defined above
data Discount = Discount { discountPercentage :: Int
                         , discountVariant :: Mode
                         , discountCopies :: Maybe Int} deriving (Eq, Show)

data Product = Product { productName :: Text
                       , productPrice :: Int } deriving (Eq, Show)

type ProductDatabase = Map Int Product
type DiscountDatabase = Map Int Discount

type Cost = Int

-- computes cost of an item given a product db in cents
itemCost :: ProductDatabase -> LineItem -> Maybe Cost
itemCost db (LineItem pid quantity) = (*) quantity <$> productPrice <$> (flip Data.Map.lookup db) pid

-- computes cost of an item with discounts applied
discountedItemCost :: ProductDatabase -> Discount -> LineItem -> Maybe Cost
discountedItemCost pdb discount li = applyDiscountToCost li discount <$> itemCost pdb li

-- applies discount percentage to cost and rounds down result
applyDiscount :: Cost -> Int -> Cost
applyDiscount cost discount = (cost * discount) `div` 100

-- decides whether to apply the given discount to the given line item
appliesToQuantity :: LineItem -> Discount -> Bool
appliesToQuantity _ (Discount _ _ Nothing) = True
appliesToQuantity (LineItem _ quantity) (Discount _ _ (Just limit)) = quantity <= limit

-- decides whether to apply the given discount to the given product id
appliesToProduct :: LineItem -> Discount -> Bool
appliesToProduct _ (Discount _ All _) = True
appliesToProduct (LineItem pid _) (Discount _ (Some list) _) = elem pid list

applyDiscountToCost :: LineItem -> Discount -> Cost -> Cost
applyDiscountToCost li discount =
  if (appliesToProduct li discount) && (appliesToQuantity li discount) then
    applyDiscount $ discountPercentage discount
  else
    id

orderCost :: ProductDatabase -> DiscountDatabase -> Order -> Cost
orderCost pdb ddb (Order items maybeDiscount) =
  Prelude.foldr (+) 0 $ catMaybes $ flip Prelude.map items $ priceFunction $ (flip Data.Map.lookup ddb) <$> maybeDiscount
  where priceFunction disc = case disc of
          Just d -> discountedItemCost pdb d
          Nothing -> itemCost pdb
