module Discounts where

import Data.Text
import Data.Map
import Data.Either (rights)

data LineItem = LineItem { lineItemProductId :: Int
                         , lineItemQuantity :: Int }

data Order = Order { orderLineItems :: [LineItem]
                   , orderDiscountId :: Maybe Int }

data Mode = All | Some [Int]

-- changed type "Variant" of discountVariant to "Mode" since that's
-- what was defined above
data Discount = Discount { discountPercentage :: Int
                         , discountVariant :: Mode
                         , discountCopies :: Maybe Int}

data Product = Product { productName :: Text
                       , productPrice :: Int }

type ProductDatabase = Map Int Product
type DiscountDatabase = Map Int Discount

type Cost = Int
data UndefinedCost =
  UnableToComputeCost
  | ProductDoesNotExist deriving (Eq, Show)

-- computes cost of an item given a product db in cents
itemCost :: ProductDatabase -> LineItem -> Either UndefinedCost Cost
itemCost db (LineItem pid quantity) =
  case Data.Map.lookup pid db of
    Just (Product _ cost) -> Right (cost * quantity)
    Nothing -> Left ProductDoesNotExist

-- computes cost of an item with discounts applied
discountedItemCost :: ProductDatabase -> Discount -> LineItem -> Either UndefinedCost Cost
discountedItemCost pdb discount li =
  case itemCost pdb li of
    Right cost -> Right (applyDiscountToCost li discount cost)
    Left err -> Left err

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
orderCost pdb ddb (Order items _) =
  Prelude.foldr (+) 0 $ rights $ Prelude.map (itemCost pdb) items
