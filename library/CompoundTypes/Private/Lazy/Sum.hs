module CompoundTypes.Private.Lazy.Sum where


data Sum2 a b =
  Sum2_1 a | Sum2_2 b

data Sum3 a b c =
  Sum3_1 a | Sum3_2 b | Sum3_3 c

data Sum4 a b c d =
  Sum4_1 a | Sum4_2 b | Sum4_3 c | Sum4_4 d

data Sum5 a b c d e =
  Sum5_1 a | Sum5_2 b | Sum5_3 c | Sum5_4 d | Sum5_5 e

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
