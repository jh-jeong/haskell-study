Haskell is...
-------------
  1. Functional
    - Functions are first-class
    - Evaluating expression rather than executing instructions
  2. Referentially transparent
  3. Lazy
  4. Statically typed

Declarations and variables
--------------------------
> x :: Int
> x = 3

-- Note that normal (non-literate) comments are preceded by two hyphens
{- or enclosed
   in curly brace/hyphen pairs. -}
-- x = 4 <-- Impossible

GHCi
----
> :load (:l)
> :reload (:r)
> :type (:t)
> :?

> ex01 = 3 + 2
> ex02 = 19 - 27
> ex03 = 2.35 * 8.6
> ex04 = 8.7 / 3.1
> ex05 = mod 19 3
> ex06 = 19 `mod` 3
> ex07 = 7 ^ 222
> ex08 = (-3) * (-7)

Defining basic functions
------------------------
> -- Compute the sum of the integers from 1 to n.
> sumtorial :: Integer -> Integer
> sumtorial 0 = 0
> sumtorial n = n + sumtorial (n-1)

Guards
------
> hailstone :: Integer -> Integer
> hailstone n
>   | n `mod` 2 == 0 = n `div` 2
>   | otherwise      = 3*n + 1

Lists
-----
> nums, range, range2 :: [Integer]
> nums   = [1,2,3,19]
> range  = [1..100]
> range2 = [2,4..100]

Constructing lists
------------------
> emptyList = []

> ex18 = 1 : []
> ex19 = 3 : (1 : [])
> ex20 = 2 : 3 : 4 : []

> ex21 = [2,3,4] == 2 : 3 : 4 : []

> -- Generate the sequence of hailstone iterations from a starting number.
> hailstoneSeq :: Integer -> [Integer]
> hailstoneSeq 1 = [1]
> hailstoneSeq n = n : hailstoneSeq (hailstone n)

Functions on lists
------------------

We can write functions on lists using *pattern matching*.

> -- Compute the length of a list of Integers.
> intListLength :: [Integer] -> Integer
> intListLength []     = 0
> intListLength (x:xs) = 1 + intListLength xs

> sumEveryTwo :: [Integer] -> [Integer]
> sumEveryTwo []         = []     -- Do nothing to the empty list
> sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
> sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs


