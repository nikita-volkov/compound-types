{-# LANGUAGE UndecidableInstances #-}
module CompoundTypes.Private.Lazy.Sum where


data Sum2 _1 _2 =
  Sum2_1 _1 | Sum2_2 _2

data Sum3 _1 _2 _3 =
  Sum3_1 _1 | Sum3_2 _2 | Sum3_3 _3

data Sum4 _1 _2 _3 _4 =
  Sum4_1 _1 | Sum4_2 _2 | Sum4_3 _3 | Sum4_4 _4

data Sum5 _1 _2 _3 _4 _5 =
  Sum5_1 _1 | Sum5_2 _2 | Sum5_3 _3 | Sum5_4 _4 | Sum5_5 _5

data Sum6 _1 _2 _3 _4 _5 _6 =
  Sum6_1 _1 | Sum6_2 _2 | Sum6_3 _3 | Sum6_4 _4 | Sum6_5 _5 | Sum6_6 _6

data Sum7 _1 _2 _3 _4 _5 _6 _7 =
  Sum7_1 _1 | Sum7_2 _2 | Sum7_3 _3 | Sum7_4 _4 | Sum7_5 _5 | Sum7_6 _6 | Sum7_7 _7


-- |
-- Automatically derives the sum-type of the according arity
-- from expressions such as:
-- 
-- > Int + Char + Bool
-- 
-- In that case it will resolve to:
-- 
-- > Sum3 Int Char Bool
type family a + b where

  Sum6 _1 _2 _3 _4 _5 _6 + _7 =
    Sum7 _1 _2 _3 _4 _5 _6 _7
  
  Sum5 _1 _2 _3 _4 _5 + Sum2 _6 _7 =
    Sum7 _1 _2 _3 _4 _5 _6 _7
  Sum5 _1 _2 _3 _4 _5 + _6 =
    Sum6 _1 _2 _3 _4 _5 _6
  
  Sum4 _1 _2 _3 _4 + Sum3 _5 _6 _7 =
    Sum7 _1 _2 _3 _4 _5 _6 _7
  Sum4 _1 _2 _3 _4 + Sum2 _5 _6 =
    Sum6 _1 _2 _3 _4 _5 _6
  Sum4 _1 _2 _3 _4 + _5 =
    Sum5 _1 _2 _3 _4 _5
  
  Sum3 _1 _2 _3 + Sum4 _4 _5 _6 _7 =
    Sum7 _1 _2 _3 _4 _5 _6 _7
  Sum3 _1 _2 _3 + Sum3 _4 _5 _6 =
    Sum6 _1 _2 _3 _4 _5 _6
  Sum3 _1 _2 _3 + Sum2 _4 _5 =
    Sum5 _1 _2 _3 _4 _5
  Sum3 _1 _2 _3 + _4 =
    Sum4 _1 _2 _3 _4
  
  Sum2 _1 _2 + Sum5 _3 _4 _5 _6 _7 =
    Sum7 _1 _2 _3 _4 _5 _6 _7
  Sum2 _1 _2 + Sum4 _3 _4 _5 _6 =
    Sum6 _1 _2 _3 _4 _5 _6
  Sum2 _1 _2 + Sum3 _3 _4 _5 =
    Sum5 _1 _2 _3 _4 _5
  Sum2 _1 _2 + Sum2 _3 _4 =
    Sum4 _1 _2 _3 _4
  Sum2 _1 _2 + _3 =
    Sum3 _1 _2 _3

  Unsubtracted _1 _2 + _2 =
    _1
  Unsubtracted _1 _2 + _3 =
    Unsubtracted (_1 + _3) _2
    
  _1 + Sum6 _2 _3 _4 _5 _6 _7 =
    Sum7 _1 _2 _3 _4 _5 _6 _7
  _1 + Sum5 _2 _3 _4 _5 _6 =
    Sum6 _1 _2 _3 _4 _5 _6
  _1 + Sum4 _2 _3 _4 _5 =
    Sum5 _1 _2 _3 _4 _5
  _1 + Sum3 _2 _3 _4 =
    Sum4 _1 _2 _3 _4
  _1 + Sum2 _2 _3 =
    Sum3 _1 _2 _3
  _1 + Unsubtracted _2 _1 =
    _2
  _1 + Unsubtracted _2 _3 =
    Unsubtracted (_1 + _2) _3
  _1 + _2 =
    Sum2 _1 _2

infixl 0 +


-- * Subtraction
-------------------------

-- |
-- An operator for removing elements from the sum-types.
-- E.g.,
-- 
-- > Int + Char + Bool - Char
-- 
-- is the same type as
-- 
-- > Int + Bool
-- 
type family a - b where

  Sum7 _1 _2 _3 _4 _5 _6 _7 - _1 =
    Sum6 _2 _3 _4 _5 _6 _7
  Sum7 _1 _2 _3 _4 _5 _6 _7 - _2 =
    Sum6 _1 _3 _4 _5 _6 _7
  Sum7 _1 _2 _3 _4 _5 _6 _7 - _3 =
    Sum6 _1 _2 _4 _5 _6 _7
  Sum7 _1 _2 _3 _4 _5 _6 _7 - _4 =
    Sum6 _1 _2 _3 _5 _6 _7
  Sum7 _1 _2 _3 _4 _5 _6 _7 - _5 =
    Sum6 _1 _2 _3 _4 _6 _7
  Sum7 _1 _2 _3 _4 _5 _6 _7 - _6 =
    Sum6 _1 _2 _3 _4 _5 _7
  Sum7 _1 _2 _3 _4 _5 _6 _7 - _7 =
    Sum6 _1 _2 _3 _4 _5 _6

  Sum6 _1 _2 _3 _4 _5 _6 - _1 =
    Sum5 _2 _3 _4 _5 _6
  Sum6 _1 _2 _3 _4 _5 _6 - _2 =
    Sum5 _1 _3 _4 _5 _6
  Sum6 _1 _2 _3 _4 _5 _6 - _3 =
    Sum5 _1 _2 _4 _5 _6
  Sum6 _1 _2 _3 _4 _5 _6 - _4 =
    Sum5 _1 _2 _3 _5 _6
  Sum6 _1 _2 _3 _4 _5 _6 - _5 =
    Sum5 _1 _2 _3 _4 _6
  Sum6 _1 _2 _3 _4 _5 _6 - _6 =
    Sum5 _1 _2 _3 _4 _5

  Sum5 _1 _2 _3 _4 _5 - _1 =
    Sum4 _2 _3 _4 _5
  Sum5 _1 _2 _3 _4 _5 - _2 =
    Sum4 _1 _3 _4 _5
  Sum5 _1 _2 _3 _4 _5 - _3 =
    Sum4 _1 _2 _4 _5
  Sum5 _1 _2 _3 _4 _5 - _4 =
    Sum4 _1 _2 _3 _5
  Sum5 _1 _2 _3 _4 _5 - _5 =
    Sum4 _1 _2 _3 _4

  Sum4 _1 _2 _3 _4 - _1 =
    Sum3 _2 _3 _4
  Sum4 _1 _2 _3 _4 - _2 =
    Sum3 _1 _3 _4
  Sum4 _1 _2 _3 _4 - _3 =
    Sum3 _1 _2 _4
  Sum4 _1 _2 _3 _4 - _4 =
    Sum3 _1 _2 _3

  Sum3 _1 _2 _3 - _1 =
    Sum2 _2 _3
  Sum3 _1 _2 _3 - _2 =
    Sum2 _1 _3
  Sum3 _1 _2 _3 - _3 =
    Sum2 _1 _2

  Sum2 _1 _2 - _1 =
    _2
  Sum2 _1 _2 - _2 =
    _1

  Unsubtracted _1 _2 - _3 =
    Unsubtracted (_1 - _3) _2

  -- This group requires the UndecidableInstances extension
  _1 - Sum7 _2 _3 _4 _5 _6 _7 _8 =
    _1 - _2 - _3 - _4 - _5 - _6 - _7 - _8
  _1 - Sum6 _2 _3 _4 _5 _6 _7 =
    _1 - _2 - _3 - _4 - _5 - _6 - _7
  _1 - Sum5 _2 _3 _4 _5 _6 =
    _1 - _2 - _3 - _4 - _5 - _6
  _1 - Sum4 _2 _3 _4 _5 =
    _1 - _2 - _3 - _4 - _5
  _1 - Sum3 _2 _3 _4 =
    _1 - _2 - _3 - _4
  _1 - Sum2 _2 _3 =
    _1 - _2 - _3

  _1 - _2 =
    Unsubtracted _1 _2

infixl 0 -


-- |
-- What you get, when the subtraction cannot yet be performed.
-- 
-- Happens when the minuend doesn't contain the subtrahend. E.g.,
-- 
-- > Char - Bool
-- 
-- produces
-- 
-- > Unsubtracted Char Bool
-- 
-- However it's possible to get back to the normal type,
-- when you perform the required addition afterwards. E.g.,
-- 
-- > Char - Bool + Bool
-- 
-- produces
-- 
-- > Char
-- 
-- This construct actually exists primarily for that purpose.
data Unsubtracted minuend subtrahend
