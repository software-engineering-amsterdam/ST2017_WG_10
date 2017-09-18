module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 120 min
data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z  | hasNegatives x y z = NoTriangle
                | hasInequality x y z = NoTriangle
                | isEquilateral x y z = Equilateral
                | isIsosceles x y z = Isosceles
                | isRectangular x y z = Rectangular
                | otherwise = Other

hasNegatives :: Integer -> Integer -> Integer -> Bool
hasNegatives x y z = length [n | n <- [x,y,z], n <= 0] > 0

hasInequality :: Integer -> Integer -> Integer -> Bool
hasInequality x y z = let ordered = sort [x,y,z] in sum (init ordered) < last ordered

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral x y z = x == y && y == z

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles x y z = (x == y && x /= z) || (x == z && x /= y) || (y == z && y /= x)

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular x y z = let ordSq = map (\n -> n * n) (sort [x,y,z]) in sum (init ordSq) == last ordSq

testNoZeros :: IO ()
testNoZeros = verboseCheck      (\x y z -> hasNegatives x y z ==> triangle x y z == NoTriangle)

testInequality :: IO ()
testInequality = verboseCheck   (\x y z -> hasInequality x y z ==> triangle x y z == NoTriangle)

testEquilateral :: IO ()
testEquilateral = verboseCheck  (\x y z -> not (hasNegatives x y z) ==> not (hasInequality x y z) ==> isEquilateral x y z ==> triangle x y z == Equilateral)

testIsosceles :: IO ()
testIsosceles = verboseCheck    (\x y z -> not (hasNegatives x y z) ==> not (hasInequality x y z) ==> isIsosceles x y z ==> triangle x y z == Isosceles)

testRectangular :: IO ()
testRectangular = verboseCheck  (\x y z -> not (hasNegatives x y z) ==> not (hasInequality x y z) ==> isRectangular x y z ==> triangle x y z == Rectangular)

testOther :: IO ()
testOther = verboseCheck        (\x y z ->  not (hasNegatives x y z) ==> 
                                            not (hasInequality x y z) ==> 
                                            not (isEquilateral x y z) ==> 
                                            not (isIsosceles x y z) ==> 
                                            not (isRectangular x y z) ==> 
                                            triangle x y z == Other)

testTriangle :: IO ()
testTriangle = verboseCheck (\x y z -> True ==> elem (triangle x y z) [NoTriangle,Equilateral,Isosceles,Rectangular,Other])

{-
	We've implemented test properties for each type of the triangles
	it uses inequality and implication to prove each type.
	
-}