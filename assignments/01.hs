module Main ( main
            ) where

import Data.String
import System.IO

{- For simplicity, all homework assignments will take on the format of haskell source file skeletons that contain questions that you will answer. e.g.

-- 1)
-- Write a function that extracts the second element of a list -}

-- secondElem :: [a] -> [a]
-- second Elem = []

-- and you will uncomment the abve two lines of starter code and replace the stub with

secondElem :: [a] -> a
secondElem l = (head . tail) l --- why does this fail if I don't use parenthesis?

-- To try out the function, you can open the command line, execute the command, "ghci", and load this script with :l 00.hs. The `main` function will nearly always contain a test script that will tell you whether or not your functions are correct.

--------------------------------
-- Beginning of Assignment 01 --
--------------------------------

{-
Explain how you would tell your grandma or a ten-year-old what a functional programming lanugage is. Top responses will be shared in class!

---your explanation here---


What are the four major reasons programmers choose Haskell?  (Hint: read )

---your explanation here---

-}

-- 2)
-- We'll start easy. Write a function that produces the next counting number.
-- e.g. successor 3 produces 4.

successor :: Int -> Int
successor n = n + 1

-- 3)
-- Write a function that computes the first n counting numbers from n down to 1.
-- e.g. countingDownNumbers 5 should produce [5, 4, 3, 2, 1]

-- I'll give this one to ya for free, so you can get in the hang of it:
countingDownNumbers :: Int -> [Int]
countingDownNumbers n
  | n <= 0 = []
  | otherwise = n:countingDownNumbers(n-1)             


-- 4)
-- Write a function that computes the first n counting numbers from 1 to n
-- e.g. countingNumbers 5 should produce [1, 2, 3, 4, 5]
-- wahoo! This ones a gift, too. Notice we had to prepend to the list, so we needed to concatenate two Lists rather than prepend a single element.

countingNumbers :: Int -> [Int]
countingNumbers n
  | n <= 0 = []
  | otherwise = (countingNumbers (n-1)) ++ [n]


-- 5)
-- Write nand
-- e.g. nand True True is False. Anything else is True.

nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

-- 6)
-- Write xor
-- e.g. xor True False is True.  xor True True is False.

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- 7)
-- Write a function to sum two squares
-- e.g. sumSquares 3 4 gives 25

sumSquares :: Int -> Int -> Int
sumSquares x y = x^2 + y^2

-- 8)
-- Write a returnDivisible :: Int -> [Int] -> [Int] function which filters a list of integers retaining only the numbers divisible by the integer passed as first argument. For integers x and n, x is divisible by n if (mod x n) == 0 (note that the test for evenness is a specific case of that).
-- e.g. returnDivisible 4 [8, 5, 12] produces [8, 12]
-- Hint: use a list comprehension

returnDivisible :: Int -> [Int] -> [Int]
returnDivisible n xs = [x | x <- xs, (mod x n) == 0]

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
main = runTests [testSecondElem, testSuccessor, testCountingDownNumbers, testCountingNumbers, testNand, testXor, testSumSquares]

testSecondElem :: Test
testSecondElem = (1, secondElem [1, 2, 3, 4] == 2)

testSuccessor :: Test
testSuccessor = (2, successor 3 == 4
                      && successor (-9) == (-8))

testCountingDownNumbers :: Test
testCountingDownNumbers = (3, (countingDownNumbers 5 == [5, 4, 3, 2, 1] 
                              && countingDownNumbers (-4) == []))

testCountingNumbers :: Test
testCountingNumbers  = (4, countingNumbers 5 == [1, 2, 3, 4, 5]
                      && countingNumbers (-4) == [])

testNand :: Test
testNand = (5, nand True True == False
               && nand False True == True)

testXor :: Test
testXor = (6, xor True True == False
              && xor False True == True)

testSumSquares :: Test
testSumSquares = (7, sumSquares 3 4 == 25)

testReturnDivisible :: Test
testReturnDivisible  = (8, returnDivisible 4 [12, 16, 3] == [12, 16])


