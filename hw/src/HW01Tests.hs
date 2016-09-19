-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, ds) = toRevDigits n == ds

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits" testToRevDigits
           [(1234, [4, 3, 2, 1]), (0, []), (-17, [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (x, y) = doubleEveryOther x == y 

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther" testDoubleEveryOther
             [([4,9,5,5], [4,18,5,10]), ([0,0], [0,0])]
             ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (x, y) = sumDigits x == y

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits" testSumDigits
             [([10,5,18,4], 19)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (cardNum, validity) = luhn cardNum == validity

ex5Tests :: [Test]
ex5Tests = [ Test "luhn" testLuhn
            [(5594589764218858, True), (1234567898765432, False)]
            ]

-- Exercise 6 -----------------------------------------

testHanoi :: ((Integer, Peg, Peg, Peg), [Move]) -> Bool
testHanoi ((n, a, b, c) , moves) = hanoi n a b c == moves

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi" testHanoi
             [((2, "a", "b", "c"), [("a","c"), ("a","b"), ("c","b")])]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
