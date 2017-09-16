--Recognizing and generating derangements - time: 40 mins
module Exercise5 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{- implemented in exercise 4 -}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []       = True
isPermutation [] _        = False
isPermutation _ []        = False
isPermutation (p:px) qx   | elem p qx = isPermutation px (delete p qx)
                          | otherwise = False

{- Give a Haskell implementation of a property isDerangement that checks whether one list is a derangement of another one. -}

-- first, we need a function to check that there are no elements in the two lists that share the same position and they're equal
checkSelf :: Eq a => [a] -> [a] -> Bool
checkSelf [] []           = True
checkSelf [] _            = False
checkSelf _ []            = False
checkSelf (x:xs) (y:ys)   | x /= y = checkSelf xs ys -- in the same position
                          | otherwise = False

-- secondly, we can implement isDerangement to check whether the elements in the same position do not match, and the lists are permutation of one another 
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = checkSelf xs ys && isPermutation xs ys

{- GHCi:
    *Exercise5> isDerangement [1,2,3] [2,3,1]
    True
    *Exercise5> isDerangement [1,2,3] [3,2,1]
    False
-}

{- Give a Haskell implementation of a function deran that generates a list of all derangements of the list [0..n-1].-}
--permutations :: [a] -> [[a]]
deran :: Int -> [[Int]]
deran n =  [xs | xs <- permutations ns, isDerangement xs ns]
           where ns = [0..(n-1)]

{-
	*Exercise5> deran 4
	[[3,2,1,0],[2,3,1,0],[1,2,3,0],[3,0,1,2],[1,3,0,2],[1,0,3,2],[3,2,0,1],[2,3,0,1],[2,0,3,1]]
-}

{- Next, define some testable properties for the isDerangement function, and use some well-chosen integer lists to test isDerangement. -}

--1
twoDerangementsOfAThird :: Eq a => [a] -> [a] -> [a] -> Bool
twoDerangementsOfAThird xs ys zs = isDerangement xs zs && isDerangement ys zs --> isDerangement xs ys

--2
isSymmetric :: Eq a =>[a] -> [a] -> Bool
isSymmetric xs ys = isDerangement xs ys --> isDerangement ys xs

--3 
isSortedNotDerangement :: Eq a => Ord a =>[a] -> [a] -> Bool
isSortedNotDerangement xs ys | (length xs /=0 && length ys /=0) = not (isDerangement (sort xs) (sort ys))
                             | otherwise = True -- to not fail in case of isSortedNotDerangement [] []

--4							 
isSelfFalse :: Eq a =>[a] -> Bool
isSelfFalse xs      | (length xs /=0 ) = not (isDerangement xs xs)
                    | otherwise = True -- to not fail in case of isSortedNotDerangement [] []

{- GHCi:
		*Exercise5> quickCheck twoDerangementsOfAThird
		+++ OK, passed 100 tests.

		*Exercise5> quickCheck isSymmetric
		+++ OK, passed 100 tests.
		
		*Exercise5> quickCheck isSortedNotDerangement
		+++ OK, passed 100 tests.
		
		*Exercise5> quickCheck isSelfFalse
		+++ OK, passed 100 tests.
-}
