module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c 
        | (a+b < c) || (a+c < b) || (b+c < a) = NoTriangle
        | (a == b) && (b == c) = Equilateral
        | (a == b) || (a == c) || (b == c) = Isosceles
        | (a^2+b^2 == c^2) || (a^2 + c^2 == b^2) || (b^2 + c^2 == a^2) = Rectangular
        | otherwise = Other

-- 3 min