module Main ( main
            ) where

import Data.String
import System.IO
import Data.Char
import Data.List

-- To try out the function, you can open the command line, execute the command, "ghci", and load this script with :l 03.hs. The `main` function will nearly always contain a test script that will tell you whether or not your functions are correct.

--------------------------------
-- Beginning of Assignment 04 --
--------------------------------
-- This assignment covers ------
-- folds -----------------------
--------------------------------

-- Note: the first four functions must all be implemented using folds

-- 1)
-- and returns True if a list of Bools are True, and False otherwise
-- remember that argument of the form x:xs must be parenthesized, and the line before a series of guards should not be appended with '='

myAnd :: [Bool] -> Bool
myAnd lst = foldl' (\a b -> a && b) True lst

-- 2)
-- or returns True if any of a list of Bools are True, and False otherwise

myOr :: [Bool] -> Bool
myOr lst = foldl' (\a b -> a || b) False lst

-- 3)
-- maximum returns the maximum element of a list (hint: max :: Ord a => a -> a -> returns the maximum of two values

myMaximum :: Ord a => [a] -> a
myMaximum lst = foldl' max (head lst) lst

-- 4)
-- reverse :: [a] -> [a], which returns a list with the elements in reverse order.
-- hint: Which fold should you use? We want to take off the first element of the list as we dive into the recursion, and then concatenate them all together in the reverse order when we pop back out of the recursion. We wouldn't be able to use foldl', because we would need to concatenate the first element with the next element right away. TODO: reword that.
-- remember that the first argument to (:) must be a single element, and both arguments to (++) must be arrays

myReverse :: [a] -> [a]
myReverse lst = foldr (\a b -> b ++ [a]) [] lst

-- for the next two functions, use faldr1. foldr1 is a variant of foldr that has no starting value argument, It uses the first element of the list as the starting element.
-- foldr1 :: (a -> a -> a) -> [a] -> a

-- 5)
-- Hint: Which fold should you use? We'll want to prepend an element to the result list one at a time.

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if p x then x : acc else acc) []


-- 6)
-- Hint: note that foldl evaluates such that the acc appears to be the last element in the list, whereas in foldr, it's the first element.
-- Head:

myHead :: [a] -> a
myHead = foldr1 (\x _ -> x)

-- 7)

myLast :: [a] -> a
myLast = foldl1 (\_ x -> x)


















-- Note that all of these are Prelude functions, so they will be always close at hand when you need them. 


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
main = runTests [testAnd, testOr, testMaximum, testReverse, testFilter, testHead, testLast]

testAnd :: Test
testAnd = (1, myAnd [True, False] == False
              && myAnd [True, True] == True)

testOr :: Test
testOr = (2, myOr [True, False] == True
               && myOr [False, False] == False)

testMaximum :: Test
testMaximum = (3, myMaximum [1, 2, 3] == 3
                    && myMaximum "hey" == 'y')

testReverse :: Test
testReverse = (4, myReverse [1, 2, 3] == [3, 2, 1])

testFilter :: Test
testFilter = (5, myFilter (>2) [1, 2, 3] == [3])

testHead :: Test
testHead = (6, myHead [1, 2, 3] == 1)

testLast :: Test
testLast = (7, myLast [1, 2, 3] == 3)