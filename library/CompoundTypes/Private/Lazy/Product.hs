module CompoundTypes.Private.Lazy.Product where


data Product2 _1 _2 =
  Product2 _1 _2

data Product3 _1 _2 _3 =
  Product3 _1 _2 _3

data Product4 _1 _2 _3 _4 =
  Product4 _1 _2 _3 _4

data Product5 _1 _2 _3 _4 _5 =
  Product5 _1 _2 _3 _4 _5

data Product6 _1 _2 _3 _4 _5 _6 =
  Product6 _1 _2 _3 _4 _5 _6

data Product7 _1 _2 _3 _4 _5 _6 _7 =
  Product7 _1 _2 _3 _4 _5 _6 _7


-- |
-- Automatically derives the product-type of the according arity
-- from expressions such as:
-- 
-- > Int * Char * Bool
-- 
-- In that case it will resolve to:
-- 
-- > Product3 Int Char Bool
type family a * b where
  Product6 _1 _2 _3 _4 _5 _6 * _7 =
    Product7 _1 _2 _3 _4 _5 _6 _7
  Product5 _1 _2 _3 _4 _5 * Product2 _6 _7 =
    Product7 _1 _2 _3 _4 _5 _6 _7
  Product5 _1 _2 _3 _4 _5 * _6 =
    Product6 _1 _2 _3 _4 _5 _6
  Product4 _1 _2 _3 _4 * Product3 _5 _6 _7 =
    Product7 _1 _2 _3 _4 _5 _6 _7
  Product4 _1 _2 _3 _4 * Product2 _5 _6 =
    Product6 _1 _2 _3 _4 _5 _6
  Product4 _1 _2 _3 _4 * _5 =
    Product5 _1 _2 _3 _4 _5
  Product3 _1 _2 _3 * Product4 _4 _5 _6 _7 =
    Product7 _1 _2 _3 _4 _5 _6 _7
  Product3 _1 _2 _3 * Product3 _4 _5 _6 =
    Product6 _1 _2 _3 _4 _5 _6
  Product3 _1 _2 _3 * Product2 _4 _5 =
    Product5 _1 _2 _3 _4 _5
  Product3 _1 _2 _3 * _4 =
    Product4 _1 _2 _3 _4
  Product2 _1 _2 * Product5 _3 _4 _5 _6 _7 =
    Product7 _1 _2 _3 _4 _5 _6 _7
  Product2 _1 _2 * Product4 _3 _4 _5 _6 =
    Product6 _1 _2 _3 _4 _5 _6
  Product2 _1 _2 * Product3 _3 _4 _5 =
    Product5 _1 _2 _3 _4 _5
  Product2 _1 _2 * Product2 _3 _4 =
    Product4 _1 _2 _3 _4
  Product2 _1 _2 * _3 =
    Product3 _1 _2 _3
  _1 * Product6 _2 _3 _4 _5 _6 _7 =
    Product7 _1 _2 _3 _4 _5 _6 _7
  _1 * Product5 _2 _3 _4 _5 _6 =
    Product6 _1 _2 _3 _4 _5 _6
  _1 * Product4 _2 _3 _4 _5 =
    Product5 _1 _2 _3 _4 _5
  _1 * Product3 _2 _3 _4 =
    Product4 _1 _2 _3 _4
  _1 * Product2 _2 _3 =
    Product3 _1 _2 _3
  _1 * _2 =
    Product2 _1 _2

infixl 1 *

