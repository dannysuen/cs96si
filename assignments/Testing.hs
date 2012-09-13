module Testing ( Test(..)
               , runTests
               ) where

import System.IO
import Data.String

type Test = (Either String String, Int)

runTest :: (Either String String, Int) -> IO ()
runTest ((Left err), num) = putStrLn $ "Failed Question #" ++ show num  ++ ". " ++ err
runTest ((Right msg), num) = putStrLn $ "Question #" ++ show num ++ " is correct! " ++ msg

runTests :: [(Either String String, Int)] -> IO ()
runTests tests = mapM_ runTest tests