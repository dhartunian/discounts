module Discounts where

import Data.Text

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

discount :: Int
discount = 0
