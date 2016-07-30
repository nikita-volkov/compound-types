module Main where

import BasePrelude
import CompoundTypes.Strict


main =
  putStrLn "This demonstration is all about the compilability"

-- |
-- This function exhibits the benefit of the first-class sum-type
-- being usable as a function parameter.
-- 
-- It also shows, how we can pattern-match it.
intCharBoolSumToString :: (Int + Char + Bool) -> String
intCharBoolSumToString =
  \case
    Sum3_1 int -> "Int: " <> show int
    Sum3_2 char -> "Char: " <> show char
    Sum3_3 bool -> "Bool: " <> show bool

-- |
-- Following is an example of a more complicated composition,
-- which is the same as the following type:
-- 
-- > Sum3 Int (Product2 Char (Sum2 Bool Double)) Char
-- 
-- Just as in the math, the product operator exhibits a higher priority.
type SumAndProductMixture =
  Int + Char * (Bool + Double) + Char

-- |
-- Where there is a multiplication and addition,
-- there naturally must be a division and subtraction!
-- 
-- Following is an example of how we can extract parts of a composite type.
-- Here the type becomes the same as the following:
-- 
-- > Bool + Double
type BoolOrDouble =
  (SumAndProductMixture - Int - Char) / Char
