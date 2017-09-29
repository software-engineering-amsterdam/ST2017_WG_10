module Lab4_3 where

import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd
import Lab4_2

-- 60 min
setIntersection :: (Ord a,Eq a) => Set a -> Set a -> Set a
setIntersection (Set a) b = list2set (filter (\x -> inSet x b) a)

setUnion :: (Ord a,Eq a) => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = list2set (a ++ b)

setDifference :: (Ord a,Eq a) => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = do
                                  let a' = filter (\x -> not(elem x b)) a
                                  let b' = filter (\x -> not(elem x a)) b
                                  list2set (a' ++ b')

----------------
-- Properties --
----------------
-- Intersection: all elements e in Set I can be found in both sets A and B
propInBothSets :: (Ord a, Eq a) => Set a -> Set a -> Set a -> Bool
propInBothSets a b (Set i) = all (\e -> (inSet e a) && (inSet e b)) i

-- Union: all elements e in Set U can be found in either set A or B
propInEitherSet :: (Ord a, Eq a) => Set a -> Set a -> Set a -> Bool
propInEitherSet a b (Set u) = all (\e -> (inSet e a) || (inSet e b)) u

-- Difference: all elements e in Set D can only be found in one of the sets A or B, but not in both
propInOnlyOneSet :: (Ord a, Eq a) => Set a -> Set a -> Set a -> Bool
propInOnlyOneSet a b (Set d) = all (\e -> not ((inSet e a) && (inSet e b))) d

-----------
-- Tests --
-----------
testIntersection :: IO ()
testIntersection = do
                    let n = 100
                    casesA <- genSets n
                    casesB <- genSets n
                    let casesAB = zip casesA casesB
                    let cases   = [(a,b,setIntersection a b) | (a,b) <- casesAB]
                    propTester_4_3_3 n propInBothSets cases


-- testIntersection
-- ...
-- "Passed on: ({-11,-2,3,9,10}, {-7,-3,4}, {})"
-- "Passed on: ({}, {}, {})"
-- "Passed on: ({-6,2,4,11,12}, {-3}, {})"
-- "Passed on: ({-2,0}, {5,7}, {})"
-- "Passed on: ({-6,-4,-2,1,2,8,9}, {}, {})"
-- "Passed all tests"

testUnion :: IO ()
testUnion = do
              let n = 100
              casesA <- genSets n
              casesB <- genSets n
              let casesAB = zip casesA casesB
              let cases   = [(a,b,setUnion a b) | (a,b) <- casesAB]
              propTester_4_3_3 n propInEitherSet cases

-- testUnion
-- ...
-- "Passed on: ({}, {-6,2}, {-6,2})"
-- "Passed on: ({-13,-2,11,13,15,17}, {5}, {-13,-2,5,11,13,15,17})"
-- "Passed on: ({-6,-3,-2,-1,1,5,6}, {-2,0,1,2,3}, {-6,-3,-2,-1,0,1,2,3,5,6})"
-- "Passed on: ({0,2}, {-2,3}, {-2,0,2,3})"
-- "Passed on: ({-8,-7,-3,-1,0,2,3,5,8}, {-2,2,3,5,6,10}, {-8,-7,-3,-2,-1,0,2,3,5,6,8,10})"
-- "Passed all tests"

testDifference :: IO ()
testDifference = do
                  let n = 100
                  casesA <- genSets n
                  casesB <- genSets n
                  let casesAB = zip casesA casesB
                  let cases   = [(a,b,setDifference a b) | (a,b) <- casesAB]
                  propTester_4_3_3 n propInEitherSet cases

-- testDifference
-- ...
-- "Passed on: ({-9,0,1,8,12}, {-10}, {-10,-9,0,1,8,12})"
-- "Passed on: ({-12,-1,12,16}, {-11,-9,-8,-6,-4,-2,7,10,13,14}, {-12,-11,-9,-8,-6,-4,-2,-1,7,10,12,13,14,16})"
-- "Passed on: ({-12,-11,-8,-5,-1,1,9,11,14}, {-16,-13,-5,2}, {-16,-13,-12,-11,-8,-1,1,2,9,11,14})"
-- "Passed on: ({0,1}, {-3,-2,-1,0,2,3}, {-3,-2,-1,1,2,3})"
-- "Passed on: ({-12,-9,-2,-1,1,7,8,9}, {0}, {-12,-9,-2,-1,0,1,7,8,9})"
-- "Passed all tests"

-- Tester
propTester_4_3_3 :: (Show a, Eq a) => Int -> (Set a -> Set a -> Set a -> Bool) -> [(Set a,Set a,Set a)] -> IO ()
propTester_4_3_3 n prop []        = print ("Passed all tests")
propTester_4_3_3 n prop (t:cases) = do
                                  let (a,b,c) = t
                                  if(prop a b c) then do
                                    print ("Passed on: ("++(show a)++", "++(show b)++", "++(show c)++")")
                                    propTester_4_3_3 (n-1) prop cases
                                  else
                                    print ("Failed on: ("++(show a)++", "++(show b)++", "++(show c)++")")

