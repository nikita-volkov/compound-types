{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
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
type family (a * b) where

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

  Undivided _1 _2 * _2 =
    _1
  Undivided _1 _2 * _3 =
    Undivided (_1 * _3) _2
    
  (*) _1 (Product6 _2 _3 _4 _5 _6 _7) =
    Product7 _1 _2 _3 _4 _5 _6 _7
  _1 * Product5 _2 _3 _4 _5 _6 =
    Product6 _1 _2 _3 _4 _5 _6
  _1 * Product4 _2 _3 _4 _5 =
    Product5 _1 _2 _3 _4 _5
  _1 * Product3 _2 _3 _4 =
    Product4 _1 _2 _3 _4
  _1 * Product2 _2 _3 =
    Product3 _1 _2 _3
  _1 * Undivided _2 _1 =
    _2
  _1 * Undivided _2 _3 =
    Undivided (_1 * _2) _3
  _1 * _2 =
    Product2 _1 _2

infixl 1 *


-- * Division
-------------------------

-- |
-- An operator for removing elements from the product-types.
-- E.g.,
-- 
-- > Int * Char * Bool / Char
-- 
-- is the same type as
-- 
-- > Int * Bool
-- 
type family a / b where

  Product7 _1 _2 _3 _4 _5 _6 _7 / _1 =
    Product6 _2 _3 _4 _5 _6 _7
  Product7 _1 _2 _3 _4 _5 _6 _7 / _2 =
    Product6 _1 _3 _4 _5 _6 _7
  Product7 _1 _2 _3 _4 _5 _6 _7 / _3 =
    Product6 _1 _2 _4 _5 _6 _7
  Product7 _1 _2 _3 _4 _5 _6 _7 / _4 =
    Product6 _1 _2 _3 _5 _6 _7
  Product7 _1 _2 _3 _4 _5 _6 _7 / _5 =
    Product6 _1 _2 _3 _4 _6 _7
  Product7 _1 _2 _3 _4 _5 _6 _7 / _6 =
    Product6 _1 _2 _3 _4 _5 _7
  Product7 _1 _2 _3 _4 _5 _6 _7 / _7 =
    Product6 _1 _2 _3 _4 _5 _6

  Product6 _1 _2 _3 _4 _5 _6 / _1 =
    Product5 _2 _3 _4 _5 _6
  Product6 _1 _2 _3 _4 _5 _6 / _2 =
    Product5 _1 _3 _4 _5 _6
  Product6 _1 _2 _3 _4 _5 _6 / _3 =
    Product5 _1 _2 _4 _5 _6
  Product6 _1 _2 _3 _4 _5 _6 / _4 =
    Product5 _1 _2 _3 _5 _6
  Product6 _1 _2 _3 _4 _5 _6 / _5 =
    Product5 _1 _2 _3 _4 _6
  Product6 _1 _2 _3 _4 _5 _6 / _6 =
    Product5 _1 _2 _3 _4 _5

  Product5 _1 _2 _3 _4 _5 / _1 =
    Product4 _2 _3 _4 _5
  Product5 _1 _2 _3 _4 _5 / _2 =
    Product4 _1 _3 _4 _5
  Product5 _1 _2 _3 _4 _5 / _3 =
    Product4 _1 _2 _4 _5
  Product5 _1 _2 _3 _4 _5 / _4 =
    Product4 _1 _2 _3 _5
  Product5 _1 _2 _3 _4 _5 / _5 =
    Product4 _1 _2 _3 _4

  Product4 _1 _2 _3 _4 / _1 =
    Product3 _2 _3 _4
  Product4 _1 _2 _3 _4 / _2 =
    Product3 _1 _3 _4
  Product4 _1 _2 _3 _4 / _3 =
    Product3 _1 _2 _4
  Product4 _1 _2 _3 _4 / _4 =
    Product3 _1 _2 _3

  Product3 _1 _2 _3 / _1 =
    Product2 _2 _3
  Product3 _1 _2 _3 / _2 =
    Product2 _1 _3
  Product3 _1 _2 _3 / _3 =
    Product2 _1 _2

  Product2 _1 _2 / _1 =
    _2
  Product2 _1 _2 / _2 =
    _1

  Undivided _1 _2 / _3 =
    Undivided (_1 / _3) _2

  -- This group requires the UndecidableInstances extension
  _1 / Product7 _2 _3 _4 _5 _6 _7 _8 =
    _1 / _2 / _3 / _4 / _5 / _6 / _7 / _8
  _1 / Product6 _2 _3 _4 _5 _6 _7 =
    _1 / _2 / _3 / _4 / _5 / _6 / _7
  _1 / Product5 _2 _3 _4 _5 _6 =
    _1 / _2 / _3 / _4 / _5 / _6
  _1 / Product4 _2 _3 _4 _5 =
    _1 / _2 / _3 / _4 / _5
  _1 / Product3 _2 _3 _4 =
    _1 / _2 / _3 / _4
  _1 / Product2 _2 _3 =
    _1 / _2 / _3

  _1 / _2 =
    Undivided _1 _2

infixl 1 /


-- |
-- What you get, when the division cannot yet be performed.
-- 
-- Happens when the dividend doesn't contain the divisor. E.g.,
-- 
-- > Char / Bool
-- 
-- produces
-- 
-- > Undivided Char Bool
-- 
-- However it's possible to get back to the normal type,
-- when you perform the required multiplication afterwards. E.g.,
-- 
-- > Char / Bool * Bool
-- 
-- produces
-- 
-- > Char
-- 
-- This construct actually exists primarily for that purpose.
data Undivided dividend divisor
