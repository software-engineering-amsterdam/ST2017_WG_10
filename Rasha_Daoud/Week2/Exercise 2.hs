--Recognizing triangles - time: 50 mins
module Exercise2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other
             deriving (Eq,Show)

{- We have several options for the defined shape type:
   1) NoTriangle
   2) Triangle: which can be: Equilateral, Isosceles, Rectangular, or Other

   We can either implement the triangle function directly.
   But I would prefer to implement each property aside & call them to determine the type of the shape in the triangle function.
   We need to make sure to check properties in the correct order using pattern matching in 'triangle' function.
-}

{- 1* Properties ---------------------------------------------------------------}

isNoTriangle :: Integer -> Integer -> Integer -> Bool
isNoTriangle x y z     | (x+y)<z || (x+z)<y || (y+z)<x   = True -- otherwise the third side can't be a line either! (0, see next condition)
                       | (x <=0 || y <=0 || z <=0)       = True -- the length of any of the sides can't be 0 for a triangle
					   | otherwise = False

-- the three sides have the same length
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral x y z | (x == y && y == z)  = True
                    | otherwise = False

-- two sides are with the same length
isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles x y z | elem x [y,z] = True
                  | elem y [x,z] = True
                  | elem z [x,y] = True
                  | otherwise = False

-- summetion of square of two sides should be equal to the square of the third side.
isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular x y z   |  x*x + y*y == z*z             = True
                      |  x*x + z*z == y*y             = True
                      |  y*y + z*z == x*x             = True
					  | otherwise = False

{- 2* triangle function -----------------------------------------------------------
   Implement a function to return a shape based on the three dimensions (Integers)
   Using pattern matching with the correct order would do the job.
   Correct order is very important. First we need to check whether the shape is really a triangle.
   e.g sides (1,1,2). That is not a triangle, even though it can fulfil the property isIsosceles
-}

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z             | isNoTriangle x y z     = NoTriangle
                           | isEquilateral x y z    = Equilateral
                           | isIsosceles x y z      = Isosceles 
                           | isRectangular x y z    = Rectangular							
                           | otherwise              = Other -- other types of triangle


{- 3* Testing correctness of the implementation ------------------------------------
 Let us consider the following shape generator:
    We pick up x, y & z from a list [1..100] using list generator.
    There will be 100 shapes that holds for isEquilateral (all sides are the same, given that we have 100 options in each list)
    We check the count, it must be 100.
-}



-- it is not possible to have a triangle with one of the sides = 0, if at least one is 0, the formula in isNoTriangle is True
testNoTriangle :: Bool
testNoTriangle = length (filter (==NoTriangle) [triangle 0 y z | y <- [1..100], z <- [1..100]]) /=0
testNoTriangle' :: Bool
testNoTriangle' = length (filter (==NoTriangle) [triangle x 0 z | x <- [1..100], z <- [1..100]]) /=0
testNoTriangle'' :: Bool
testNoTriangle'' = length (filter (==NoTriangle) [triangle x y 0 | x <- [1..100], y <- [1..100]]) /=0

{-testNoTriangleC :: Bool
testNoTriangleC = do
                 l <- genIntList
				 return (testNoTriangle l) -}

testEquilateral :: Bool
testEquilateral = length (filter (==Equilateral) [triangle x y z | x <- [1..100], y <- [1..100], z <- [1..100]]) == 100

testIsosceles :: Bool
testIsosceles = length (filter (==Isosceles) [triangle 100 100 z |  z <- [1..99]]) == 99 -- to guarnatee it is not a Equilateral

testRectangular :: Bool
testRectangular = length (filter (==Rectangular) [ triangle x 4 3 | x <- [1..100]]) == 1
--the only possibility is that x = 5

-- in this case, the count of other should be: 
-- the sum of all generated shapes - the sum of all recognized shapes in previous tests
otherCount :: Int
otherCount = length[triangle x y z | x <- [0..100], y <- [1..100], z <- [1..100]]
          - length (filter (==NoTriangle) [triangle 0 y z | y <- [1..100], z <- [1..100]])
		  - length (filter (==Equilateral) [triangle x y z | x <- [1..100], y <- [1..100], z <- [1..100]])
		  - length (filter (==Rectangular) [ triangle x 4 3 | x <- [1..100]])
		  - length (filter (==Isosceles) [triangle 100 100 z |  z <- [1..99]])

testOther :: Bool
testOther =   (length [triangle x y z | x <- [1..100], y <- [1..100], z <- [1..100]]
              -
			  length (filter (==Other) [triangle x y z | x <- [1..100], y <- [1..100], z <- [1..100]]))
			  == otherCount
--1^2 + 3^2 = 10 (5^2)
{- GHCi:
   *Exercise2> testNoTriangle
   True
   *Exercise2> testEquilateral
   True
   *Exercise2> testIsosceles
   True
   *Exercise2> testRectangular
   True
-}

main = do
	print testNoTriangle
	print testEquilateral
	print testIsosceles
	print testRectangular

{- *Exercise2> main
	True
	True
	True
	True
-}