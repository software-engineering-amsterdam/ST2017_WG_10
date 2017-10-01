module Lab4_2 where

import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd
import Control.Monad

-- Scratch:     30 min
-- QuickCheck:  60 min

-- Taken from lecture 2
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- Taken from lecture 2
randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

-- Taken from lecture 2
genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

-- Taken from lecture 2
getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

-----------------
-- Generator 1 --
-- genSets n   --
-----------------
genSet :: IO (Set Int)
genSet = do
            xs <- genIntList
            return (list2set xs)

genSets :: Int -> IO [Set Int]
genSets n | n <= 0    = return []
          | otherwise = do
                          s     <- genSet
                          sets  <- genSets (n-1)
                          return (s:sets)

-----------------
-- Generator 2 --
-- QuickCheck  --
-----------------
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
 arbitrary = do 
              xs <- (listOf arbitrary)
              return (list2set xs)

----------------
-- Properties --
----------------
propIsOrdered :: Set Int -> Bool
propIsOrdered (Set xs) = (sort xs) == xs

propHasNoDuplicates :: Set Int -> Bool
propHasNoDuplicates (Set xs) = (nub xs) == xs

propIsSet :: Set Int -> Bool
propIsSet set = (propIsOrdered set) && (propHasNoDuplicates set)

-----------
-- Tests --
-----------
testIsOrdered :: IO ()
testIsOrdered = do
                  let n = 100
                  cases <- genSets n
                  propTester n propIsOrdered cases
-- testIsOrdered
-- ...
-- "Passed on: {-7,-6,0,2,7}"
-- "Passed all tests"

qcTestIsOrdered :: IO ()
qcTestIsOrdered = verboseCheck (\c -> propIsOrdered c)
-- qcTestIsOrdered
-- ...
-- Passed:
-- {-99,-96,-93,-92,-90,-86,-81,-79,-70,-67,-65,-63,-59,-48,-46,-45,-44,-42,-27,-24,-22,-21,-19,-12,0,2,3,11,18,20,28,41,43,49,51,57,58,61,62,67,71,76,80,81,83,87,95,97}
--
-- +++ OK, passed 100 tests.

testHasNoDuplicates :: IO ()
testHasNoDuplicates = do
                        let n = 100
                        cases <- genSets n
                        propTester n propHasNoDuplicates cases
-- testHasNoDuplicates
-- ...
-- "Passed on: {-2,-1,0,3,4,5}"
-- "Passed all tests"

qcTestHasNoDuplicates :: IO ()
qcTestHasNoDuplicates = verboseCheck (\c -> propHasNoDuplicates c)
-- qcTestHasNoDuplicates
-- ...
-- Passed:
-- {-79,-72,-16,26,53,63}
--
-- +++ OK, passed 100 tests.

testIsSet :: IO ()
testIsSet = do
              let n = 100
              cases <- genSets n
              propTester n propIsSet cases
-- testIsSet
-- ...
-- "Passed on: {-11,-7,0,1,3,5}"
-- "Passed all tests"

qcTestIsSet :: IO ()
qcTestIsSet = verboseCheck (\c -> propIsOrdered c ==> propHasNoDuplicates c)
-- qcTestIsSet
-- ...
-- Passed:
-- {-98,-97,-92,-87,-85,-60,-58,-56,-47,-41,-34,-27,-12,-6,-5,8,9,15,16,19,26,33,43,63,66,67,69,71,82,92,97}
--
-- +++ OK, passed 100 tests.

propTester :: Int -> (Set Int -> Bool) -> [Set Int] -> IO ()
propTester n prop []        = print ("Passed all tests")
propTester n prop (c:cases) = do
                                if(prop c) then do
                                  print ("Passed on: " ++ (show c))
                                  propTester (n-1) prop cases
                                else
                                  print ("Failed on: " ++ (show c))
