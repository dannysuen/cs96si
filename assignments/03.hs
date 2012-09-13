module Main ( main
            ) where

import Data.String
import System.IO
import Data.Char

-- To try out the function, you can open the command line, execute the command, "ghci", and load this script with :l 03.hs. The `main` function will nearly always contain a test script that will tell you whether or not your functions are correct.

--------------------------------
-- Beginning of Assignment 03 --
--------------------------------
-- This assignment covers ------
-- currying, sections, ---------
-- higher order functions ------
-- map, lambdass ---------------
--------------------------------

{-
What's the difference between higher order functions and first-class functions?

* "higher-order" describes a mathematical concept of functions that operate on other functions
* "First-class" is a computer science term that describes programming language entities that have no restriction on their use.

-}

{-
Explain how the following function works:

Prelude> map (uncurry (*)) [(1,2),(3,4),(5,6)]
[2,12,30]

Hint: Here are the signatures of the Prelude functions "curry" and "uncurry"

curry :: ((a, b) -> c) -> a -> b -> cSource
--curry converts an uncurried function to a curried function.

uncurry :: (a -> b -> c) -> (a, b) -> cSource
--uncurry converts a curried function to a function on pairs.
-}


-- 1)

-- Write the function "add5" such that it gives the sum of 5 and any other number. Write a definition of add5 that uses partial application, and one that uses lambda notation.

add5 :: Int -> Int
--add5 = add 5
add5 = \y -> 5 + y


-- 2)

-- Write the function "startsWithW" that filters all the words in a list such that it keeps only the words that begin with the letter 'W'.

startsWithW :: [String] -> [String]
startsWithW wordsList = filter (\str -> head str == 'W') wordsList

-- 3)

-- Write the function "allCaps" which capitalizes all letters in a string
-- Hint: I imported Data.Char for this function

allCaps :: String -> [Char]
allCaps str = map toUpper str

-- 4)

-- Write a function that doubles integers, using a section (e.g. a (*2)

doubleMe :: [Int] -> [Int]
doubleMe numbers = map (*2) numbers

--------------------------------------------------
-- Testing functions. Nothing to see here... -----
--------------------------------------------------

type Test = (Int, Bool)

runTest :: (Int, Bool) -> IO ()
runTest (num, isCorrect)
  | isCorrect = putStrLn $ "Question #" ++ show num ++ " is correct!"
  | otherwise = putStrLn $ "Failed Question #" ++ show num ++ ". "

runTests :: [(Int, Bool)] -> IO ()
runTests tests = mapM_ runTest tests

main :: IO ()
main = runTests [testAdd5, testStartsWithW, testAllCaps, testDoubleMe]

testAdd5 :: Test
testAdd5 = (1, add5 10 == 15)

testStartsWithW :: Test
testStartsWithW = (2, startsWithW ["Whale", "rose", "What", "apple"] == ["Whale", "What"])

testAllCaps :: Test
testAllCaps = (3, allCaps "hey" == "HEY")

testDoubleMe :: Test
testDoubleMe = (4, doubleMe [1, 2, 3] == [2, 4, 6])