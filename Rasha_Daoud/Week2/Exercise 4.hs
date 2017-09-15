--Recognizing Permutations - time: 
module Exercise4 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []       = True
isPermutation [] _        = False
isPermutation _ []        = False
isPermutation (p:px) qx   | elem p qx = isPermutation px (delete p qx)
                          | otherwise = False

-- first we need a conversion function from integer to a list of all digits of that integer 
int2ListInt :: Integer -> [Integer]
int2ListInt 0 = []
int2ListInt n | (n<10) = [n] 
              | otherwise = int2ListInt (div n 10) ++ [mod n 10] 
{-Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation. You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?-}

isCommutative :: Ord a => [a] -> [a] -> Bool
isCommutative xs ys = sort xs == sort ys

isAssosiative :: Eq a => Num a => [a] -> [a] -> Bool
isAssosiative xs ys = sum xs == sum ys

HasSameLength : Ord a => [a] -> [a] -> Bool
HasSameLength xs ys = length xs == length ys