module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 20 min
rot13 :: Eq a => [a] -> [a]
rot13 xs = rotN xs 13

rotN :: Eq a => [a] -> Int -> [a]
rotN xs n | (n == 1)  = rot xs
          | (n > 1)   = rot (rotN xs (n-1))
          | otherwise = xs

rot :: Eq a => [a] -> [a]
rot xs = tail xs ++ [head xs]