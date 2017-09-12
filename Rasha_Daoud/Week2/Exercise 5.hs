--Recognizing and generating derangements - time: 
module Exercise5 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

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
checkSelf (x:xs) (y:ys)   | x /= y = checkSelf xs ys -- no match in the same position
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