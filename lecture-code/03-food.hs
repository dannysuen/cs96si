module Food ( main
            , Color(..)
            , Fruit(..)
            , Price
            , Pounds
            ) where

import Data.String
import Data.List
import Data.Function
import System.IO

data Color = Red | Green | Blue | Yellow
  deriving (Eq, Show, Read, Ord, Enum)

type Price = Float
type Pounds = Float

data Fruit = Fruit { color :: Color
                   , price :: Price
                   , weight :: Pounds
                   }

data Steak = Steak

instance Food Fruit where
  pricePerPound fruit = (price fruit) / (weight fruit)

instance Food Steak where
  pricePerPound steak = 20.0

-- Let's say that anything that is a Food must implement the pricePerPound function.
class Food food where
  pricePerPound :: food -> Price


instance Show Fruit where
  show (Fruit c p w) = "Color: " ++ show c ++ ". Price per pound: " ++ show (pricePerPound (Fruit c p w))
  
main :: IO ()
main = let banana = Fruit { color = Yellow, price = 3.14, weight = 0.3 }
           strawberry = Fruit { color = Red, price = 4.99, weight = 0.43 }
           kiwi = Fruit { color = Green, price = 3.00, weight = 0.8 }
           fruitSortedByColor = sortBy (compare `on` color) [banana, strawberry, kiwi]
           fruitSortedByPrice = sortBy (compare `on` pricePerPound) [banana, strawberry, kiwi]
       in do putStrLn $ "The fruit sorted by color is " ++ show fruitSortedByColor
             putStrLn $ "The fruit sorted by price is " ++ show fruitSortedByPrice
             putStrLn $ "Red is " ++ show Red
             putStrLn $ "Does Yellow == Red? " ++ show (Yellow == Red)
             putStrLn $ "Does Red == Red? " ++ show (Red == Red)
           

-- Note that the $ operator is for avoiding parenthesis. Anything appearing after it will take precedence over anything that comes before.
-- We could equivalently have written putStrLn ("the fruit grouped by color is " ++ show fruitGroupedByColor