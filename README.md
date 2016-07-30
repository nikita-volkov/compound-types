## Intro

This library provides first-class multi-arity product- and sum-types and neat type-level utilities for their composition. The solution is quite simple and doesn’t require the advanced proficiency in the language to be applied in practice.

Here's an example of what you can do with it:

```haskell
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
-- > Sum3 Int (Product2 Char (Sum2 Bool String)) Char
-- 
-- Just as in the math, the product operator exhibits a higher priority.
type SumAndProductMixture =
  Int + Char * (Bool + String) + Char

-- |
-- Where there is a multiplication and addition,
-- there naturally must be a division and subtraction!
-- 
-- Following is an example of how we can extract parts of a composite type.
-- Here the type becomes the same as the following:
-- 
-- > Bool + String
type BoolOrString =
  (SumAndProductMixture - Int - Char) / Char
```

## Links

* [On Hackage](http://hackage.haskell.org/package/compound-types)

* [A blog-post with the comprehensive introduction](http://nikita-volkov.github.io/first-class-sums-and-products/)
