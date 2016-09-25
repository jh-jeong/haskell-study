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
> -- Compute the length of a list of Integers.
> intListLength :: [Integer] -> Integer
> intListLength []     = 0
> intListLength (x:xs) = 1 + intListLength xs

> sumEveryTwo :: [Integer] -> [Integer]
> sumEveryTwo []         = []     -- Do nothing to the empty list
> sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
> sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

let expressions
---------------
> strLength :: String -> Int
> strLength []     = 0
> strLength (_:xs) = let len_rest = strLength xs in
>                    len_rest + 1

where clauses
-------------
> frob :: String -> Char
> frob []  = 'a'   -- len is NOT in scope here
> frob str
>   | len > 5   = 'x'
>   | len < 3   = 'y'
>   | otherwise = 'z'
>   where
>     len = strLength str

case expressions
----------------
> ex03 = case "Hello" of
>            []      -> 3
>            ('H':s) -> length s
>            _       -> 7

> failureToZero' :: FailableDouble -> Double
> failureToZero' x = case x of
>                      Failure -> 0
>                      OK d    -> d

Recursion patterns
------------------
1. Map
> map (+1) exampleList
> map abs  exampleList
> map (^2) exampleList

2. Filter
> filter :: (a -> Bool) -> [a] -> [a]
> filte _ [] = []
> filter p (x:xs)
>   | p x       = x : filter p xs
>   | otherwise = filter p xs

3. Fold
> fold f z [a,b,c] == a `f` (b `f` (c `f` z))

Functional combinators
----------------------
1. function composition
> (.) :: (b -> c) -> (a -> b) -> a -> c
> (.) f g x = f (g x)

> add1Mul4 :: [Int] -> [Int]
> add1Mul4 x = map ((*4) . (+1)) x

2. avoiding parentheses
> ($) :: (a -> b) -> a -> b
> f $ x = f x

> negateNumEvens2 :: [Int] -> Int
> negateNumEvens2 x = negate $ length $ filter even x

Lambda
------
> duplicate2 :: [String] -> [String]
> duplicate2 = map (\x -> x ++ x)

Enumeration types
-----------------
> data Thing = Shoe 
>            | Ship 
>            | SealingWax 
>            | Cabbage 
>            | King
>   deriving Show

> shoe :: Thing
> shoe = Shoe
>
> listO'Things :: [Thing]
> listO'Things = [Shoe, SealingWax, King, Cabbage, King]

Algebraic data types in general
-------------------------------
data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4

* Pattern matching
pat ::= _
     |  var
     |  var @ ( pat )
     |  ( Constructor pat1 pat2 ... patn )
    
* Recursive data type
data List t = Empty | Cons t (List t)
