module Main where

import Rebase.Prelude
import CompoundTypes.Strict


main =
  error "This demonstration is all about the compilability"

-- |
-- Same as the following type: 
-- 
-- > Sum3 Int Char Bool
type IntCharBoolSum =
  Int + Char + Bool

-- |
-- How it can be pattern-matched
intCharBoolSumToText :: IntCharBoolSum -> String
intCharBoolSumToText =
  \case
    Sum3_1 int -> "Int: " <> show int
    Sum3_2 char -> "Char: " <> show char
    Sum3_3 bool -> "Bool: " <> show bool

-- |
-- Same as the following type:
-- 
-- > Sum3 Int (Product2 Char (Sum2 Bool String)) Char
-- 
-- Just as in the math, the product operator exhibits a higher priority.
type SumAndProductMixture =
  Int + Char * (Bool + String) + Char
