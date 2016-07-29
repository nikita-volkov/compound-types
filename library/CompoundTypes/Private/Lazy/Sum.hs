module CompoundTypes.Private.Lazy.Sum where


data Sum2 v1 v2 =
  Sum2_1 v1 | Sum2_2 v2

data Sum3 v1 v2 v3 =
  Sum3_1 v1 | Sum3_2 v2 | Sum3_3 v3

data Sum4 v1 v2 v3 v4 =
  Sum4_1 v1 | Sum4_2 v2 | Sum4_3 v3 | Sum4_4 v4

data Sum5 v1 v2 v3 v4 v5 =
  Sum5_1 v1 | Sum5_2 v2 | Sum5_3 v3 | Sum5_4 v4 | Sum5_5 v5

data Sum6 v1 v2 v3 v4 v5 v6 =
  Sum6_1 v1 | Sum6_2 v2 | Sum6_3 v3 | Sum6_4 v4 | Sum6_5 v5 | Sum6_6 v6

data Sum7 v1 v2 v3 v4 v5 v6 v7 =
  Sum7_1 v1 | Sum7_2 v2 | Sum7_3 v3 | Sum7_4 v4 | Sum7_5 v5 | Sum7_6 v6 | Sum7_7 v7


-- |
-- Automatically derives the sum-type of the according arity
-- from expressions such as:
-- 
-- > Int + Char + Bool
-- 
-- In that case it will resolve to:
-- 
-- > Sum3 Int Char Bool
type family (+) a b where
  Sum6 v1 v2 v3 v4 v5 v6 + v7 =
    Sum7 v1 v2 v3 v4 v5 v6 v7
  Sum5 v1 v2 v3 v4 v5 + Sum2 v6 v7 =
    Sum7 v1 v2 v3 v4 v5 v6 v7
  Sum5 v1 v2 v3 v4 v5 + v6 =
    Sum6 v1 v2 v3 v4 v5 v6
  Sum4 v1 v2 v3 v4 + Sum3 v5 v6 v7 =
    Sum7 v1 v2 v3 v4 v5 v6 v7
  Sum4 v1 v2 v3 v4 + Sum2 v5 v6 =
    Sum6 v1 v2 v3 v4 v5 v6
  Sum4 v1 v2 v3 v4 + v5 =
    Sum5 v1 v2 v3 v4 v5
  Sum3 v1 v2 v3 + Sum4 v4 v5 v6 v7 =
    Sum7 v1 v2 v3 v4 v5 v6 v7
  Sum3 v1 v2 v3 + Sum3 v4 v5 v6 =
    Sum6 v1 v2 v3 v4 v5 v6
  Sum3 v1 v2 v3 + Sum2 v4 v5 =
    Sum5 v1 v2 v3 v4 v5
  Sum3 v1 v2 v3 + v4 =
    Sum4 v1 v2 v3 v4
  Sum2 v1 v2 + Sum5 v3 v4 v5 v6 v7 =
    Sum7 v1 v2 v3 v4 v5 v6 v7
  Sum2 v1 v2 + Sum4 v3 v4 v5 v6 =
    Sum6 v1 v2 v3 v4 v5 v6
  Sum2 v1 v2 + Sum3 v3 v4 v5 =
    Sum5 v1 v2 v3 v4 v5
  Sum2 v1 v2 + Sum2 v3 v4 =
    Sum4 v1 v2 v3 v4
  Sum2 v1 v2 + v3 =
    Sum3 v1 v2 v3
  v1 + Sum6 v2 v3 v4 v5 v6 v7 =
    Sum7 v1 v2 v3 v4 v5 v6 v7
  v1 + Sum5 v2 v3 v4 v5 v6 =
    Sum6 v1 v2 v3 v4 v5 v6
  v1 + Sum4 v2 v3 v4 v5 =
    Sum5 v1 v2 v3 v4 v5
  v1 + Sum3 v2 v3 v4 =
    Sum4 v1 v2 v3 v4
  v1 + Sum2 v2 v3 =
    Sum3 v1 v2 v3
  v1 + v2 =
    Sum2 v1 v2

infixl 0 +
