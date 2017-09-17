module Lab2 where



import Data.List

import Data.Char

import System.Random

import Test.QuickCheck



-- 30 min



data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)



isNoTriangle :: Integer -> Integer -> Integer -> Bool

isNoTriangle x y z =  if(length (filter (\x -> x == 0) [x,y,z]) > 0) then 

                        True

                      else

                        do

                            let abc = sort [x,y,z]

                            let a = abc !! 0

                            let b = abc !! 1

                            let c = abc !! 2

                            if(a+b >= c) then False else True

                        

isEquilateral :: Integer -> Integer -> Integer -> Bool

isEquilateral x y z = if(x == y && y == z) then True else False



isIsosceles :: Integer -> Integer -> Integer -> Bool

isIsosceles x y z = if(x == y || y == z || x == z) then True else False



isRectangular  :: Integer -> Integer -> Integer -> Bool

isRectangular x y z = do

                        let abc = sort [x,y,z]

                        let a = abc !! 0

                        let b = abc !! 1

                        let c = abc !! 2

                        if((a^2) + (b^2) == (c^2)) then True else False

                        

triangle :: Integer -> Integer -> Integer -> Shape

triangle x y z  | isNoTriangle x y z = NoTriangle

                | isEquilateral x y z = Equilateral

                | isIsosceles x y z = Isosceles

                | isRectangular x y z = Rectangular

                | otherwise = Other