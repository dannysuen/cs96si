module Main ( main
            ) where

import Data.String
import System.IO
import Data.Char
import Data.List

-- To try out the function, you can open the command line, execute the command, "ghci", and load this script with :l 03.hs. The `main` function will nearly always contain a test script that will tell you whether or not your functions are correct.

--------------------------------
-- Beginning of Assignment 06 --
--------------------------------
-- This assignment covers ------
-- Monads and do-block  --------
-- notation --------------------
--------------------------------

{-



-}








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
main = runTests []

testAnd :: Test
testAnd = (1, myAnd [True, False] == False
              && myAnd [True, True] == True)
