module Main ( main
            ) where

import Data.String
import System.IO

-- To try out the function, you can open the command line, execute the command, "ghci", and load this script with :l 00.hs. The `main` function will nearly always contain a test script that will tell you whether or not your functions are correct.

--------------------------------
-- Beginning of Assignment 02 --
--------------------------------
-- This assignment covers ------
-- types, class constraints-, --
-- record syntax, --------- ----
-- recursion, lazy evaluation --

{-

1. Describe the difference between the keywords "data" "type" and "newtype".

If you just want your type signatures to look cleaner and be more descriptive, you probably want type synonyms. If you want to take an existing type and wrap it in a new type in order to make it an instance of a type class, chances are you're looking for a newtype. And if you want to make something completely new, odds are good that you're looking for the data keyword.

-}


{-

Change the definition of the food class below such that it implements pricePerKilo as well as pricePerPound. Add a new data type, "Kilograms", and compute the pricePerKilo based no the pricePerPound. You don't need to modify the interfaces of the Food instances.

-}


data Color = Red | Green | Blue | Yellow
  deriving (Eq, Show, Read, Ord, Enum)

type Price = Float
type Pounds = Float
type Kilograms = Float

data Steak = Steak { name :: String
                   , cut :: String
                   }

instance Food Fruit where
  pricePerPound fruit = (price fruit) / (weight fruit)

instance Food Steak where
  pricePerPound steak = 20.0

-- Let's say that anything that is a Food must implement the pricePerPound function.             
class Food food where
  pricePerPound :: food -> Price
  pricePerKilo :: food -> Price
  pricePerKilo f = (pricePerPound f) * 0.43592

data Fruit = Fruit { color :: Color
                   , price :: Price
                   , weight :: Pounds
                   } 

-- Let's say that anything that is a Food must implement the pricePerPound function.             

instance Show Fruit where
  show (Fruit c p w) = "Color: " ++ show c ++ ". Price per pound: " ++ show (pricePerPound (Fruit c p w))


{-

Give the Steak data type some interesting fields (e.g. name, cut), and make itan instance of the Show type class.

-}

instance Show Steak where
  show (Steak n c) = "The steak is named " ++ n ++ " and the cut is " ++ c

{-

Write a function called isExpensive that takes a Food and returns True if the Food's price per pound is greater than 5 and False otherwise.

Hint: Use a class constraint to ensure the input implements the pricePerPound function.

-}

isExpensive :: (Food f) => f -> Bool
isExpensive food = pricePerPound food > 5

{-
from http://en.wikibooks.org/wiki/Haskell/Recursion

Give recursive definitions for the following list-based functions. In each case, think what the base case would be, then think what the general case would look like, in terms of everything smaller than it. (Note that all of these functions are available in Prelude, so you will want to give them different names when testing your definitions in GHCi.)
-}

-- 1)
{-
replicate :: Int -> a -> [a], which takes an element and a count and returns the list which is that element repeated that many times. E.g. replicate 3 'a' = "aaa". (Hint: think about what replicate of anything with a count of 0 should be; a count of 0 is your 'base case'.)
-}

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x:(myReplicate (n-1) x)


-- 2)
{-
(!!) :: [a] -> Int -> a, which returns the element at the given 'index'. The first element is at index 0, the second at index 1, and so on. Note that with this function, you're recursing both numerically and down a list[4].
-}


myIndex :: [a] -> Int -> a
myIndex (x:xs) 0 = x
myIndex (x:xs) n = myIndex xs (n-1)


-- 3)
{-
zip :: [a] -> [b] -> [(a, b)], which takes two lists and 'zips' them together, so that the first pair in the resulting list is the first two elements of the two lists, and so on. E.g. zip [1,2,3] "abc" = [(1, 'a'), (2, 'b'), (3, 'c')]. If either of the lists is shorter than the other, you can stop once either list runs out. E.g. zip [1,2] "abc" = [(1, 'a'), (2, 'b')].
-}

-- recall that arguments of the form (x:xs) must be surrounded by parenthesis, or you'll get this error: "Parse error in pattern"

myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x, y):(zip xs ys)
myZip _ _ = []

--------------------------------------------------
-- Testing functions. Nothing to see here... -----
--------------------------------------------------

type Test = (Int, Bool)

runTest :: Test -> IO ()
runTest (num, isCorrect)
  | isCorrect = putStrLn $ "Question #" ++ show num ++ " is correct!"
  | otherwise = putStrLn $ "Failed Question #" ++ show num ++ ". "

runTests :: [Test] -> IO ()
runTests tests = mapM_ runTest tests

main :: IO ()
main = runTests [testReplicate, testIndex, testZip, testFood]
                
testReplicate :: Test
testReplicate = (1, (myReplicate 5 'a') == "aaaaa")

testIndex :: Test
testIndex = (2, myIndex [1, 3, 5] 2 == 5)

testZip :: Test
testZip = (3, (myZip [1, 2, 3] ['a', 'b', 'c'] == [(1, 'a'), (2, 'b'), (3, 'c')]))

testFood :: Test
testFood = (55, (let banana = Fruit { color = Yellow, price = 3.14, weight = 0.3 }
                     strawberry = Fruit { color = Red, price = 4.99, weight = 0.43 }
                     kiwi = Fruit { color = Green, price = 3.00, weight = 0.8 }
                 in (isExpensive strawberry == True && isExpensive kiwi == False && isExpensive banana == True)))


