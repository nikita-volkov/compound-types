module CompoundTypes.Private.Strict.Sum where


data Sum2 v1 v2 =
  Sum2_1 !v1 | Sum2_2 !v2

data Sum3 v1 v2 v3 =
  Sum3_1 !v1 | Sum3_2 !v2 | Sum3_3 !v3

data Sum4 v1 v2 v3 v4 =
  Sum4_1 !v1 | Sum4_2 !v2 | Sum4_3 !v3 | Sum4_4 !v4

data Sum5 v1 v2 v3 v4 v5 =
  Sum5_1 !v1 | Sum5_2 !v2 | Sum5_3 !v3 | Sum5_4 !v4 | Sum5_5 !v5

-- |
-- Automatically derives the sum-type of the according arity
-- from expressions such as:
-- 
-- > (Int + Char + Bool)
-- 
-- In that case it will resolve to:
-- 
-- > Sum3 Int Char Bool
type family (+) a b where
  Sum4 v1 v2 v3 v4 + v5 =
    Sum5 v1 v2 v3 v4 v5
  Sum3 v1 v2 v3 + Sum2 v4 v5 =
    Sum5 v1 v2 v3 v4 v5
  Sum3 v1 v2 v3 + v4 =
    Sum4 v1 v2 v3 v4
  Sum2 v1 v2 + Sum3 v3 v4 v5 =
    Sum5 v1 v2 v3 v4 v5
  Sum2 v1 v2 + Sum2 v3 v4 =
    Sum4 v1 v2 v3 v4
  Sum2 v1 v2 + v3 =
    Sum3 v1 v2 v3
  v1 + Sum4 v2 v3 v4 v5 =
    Sum5 v1 v2 v3 v4 v5
  v1 + Sum3 v2 v3 v4 =
    Sum4 v1 v2 v3 v4
  v1 + Sum2 v2 v3 =
    Sum3 v1 v2 v3
  v1 + v2 =
    Sum2 v1 v2

infixl 0 +
