module Discounts where

import Data.Text
import Data.Map

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

-- computes cost of an item given a product db in cents
itemCost :: ProductDatabase -> LineItem -> Int
itemCost _ _ = 0
