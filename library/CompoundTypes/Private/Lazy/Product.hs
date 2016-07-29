module CompoundTypes.Private.Lazy.Product where


data Product2 v1 v2 =
  Product2 v1 v2

data Product3 v1 v2 v3 =
  Product3 v1 v2 v3

data Product4 v1 v2 v3 v4 =
  Product4 v1 v2 v3 v4

data Product5 v1 v2 v3 v4 v5 =
  Product5 v1 v2 v3 v4 v5

data Product6 v1 v2 v3 v4 v5 v6 =
  Product6 v1 v2 v3 v4 v5 v6

data Product7 v1 v2 v3 v4 v5 v6 v7 =
  Product7 v1 v2 v3 v4 v5 v6 v7


-- |
-- Automatically derives the product-type of the according arity
-- from expressions such as:
-- 
-- > (Int * Char * Bool)
-- 
-- In that case it will resolve to:
-- 
-- > Product3 Int Char Bool
type family (*) a b where
  Product6 v1 v2 v3 v4 v5 v6 * v7 =
    Product7 v1 v2 v3 v4 v5 v6 v7
  Product5 v1 v2 v3 v4 v5 * Product2 v6 v7 =
    Product7 v1 v2 v3 v4 v5 v6 v7
  Product5 v1 v2 v3 v4 v5 * v6 =
    Product6 v1 v2 v3 v4 v5 v6
  Product4 v1 v2 v3 v4 * Product3 v5 v6 v7 =
    Product7 v1 v2 v3 v4 v5 v6 v7
  Product4 v1 v2 v3 v4 * Product2 v5 v6 =
    Product6 v1 v2 v3 v4 v5 v6
  Product4 v1 v2 v3 v4 * v5 =
    Product5 v1 v2 v3 v4 v5
  Product3 v1 v2 v3 * Product4 v4 v5 v6 v7 =
    Product7 v1 v2 v3 v4 v5 v6 v7
  Product3 v1 v2 v3 * Product3 v4 v5 v6 =
    Product6 v1 v2 v3 v4 v5 v6
  Product3 v1 v2 v3 * Product2 v4 v5 =
    Product5 v1 v2 v3 v4 v5
  Product3 v1 v2 v3 * v4 =
    Product4 v1 v2 v3 v4
  Product2 v1 v2 * Product5 v3 v4 v5 v6 v7 =
    Product7 v1 v2 v3 v4 v5 v6 v7
  Product2 v1 v2 * Product4 v3 v4 v5 v6 =
    Product6 v1 v2 v3 v4 v5 v6
  Product2 v1 v2 * Product3 v3 v4 v5 =
    Product5 v1 v2 v3 v4 v5
  Product2 v1 v2 * Product2 v3 v4 =
    Product4 v1 v2 v3 v4
  Product2 v1 v2 * v3 =
    Product3 v1 v2 v3
  v1 * Product6 v2 v3 v4 v5 v6 v7 =
    Product7 v1 v2 v3 v4 v5 v6 v7
  v1 * Product5 v2 v3 v4 v5 v6 =
    Product6 v1 v2 v3 v4 v5 v6
  v1 * Product4 v2 v3 v4 v5 =
    Product5 v1 v2 v3 v4 v5
  v1 * Product3 v2 v3 v4 =
    Product4 v1 v2 v3 v4
  v1 * Product2 v2 v3 =
    Product3 v1 v2 v3
  v1 * v2 =
    Product2 v1 v2

infixl 1 *
