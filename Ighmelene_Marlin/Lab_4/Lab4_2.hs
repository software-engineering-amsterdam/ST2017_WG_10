module Lab4_2 where

import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd

-- Scratch:     30 min

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

testHasNoDuplicates :: IO ()
testHasNoDuplicates = do
                        let n = 100
                        cases <- genSets n
                        propTester n propHasNoDuplicates cases

testIsSet :: IO ()
testIsSet = do
              let n = 100
              cases <- genSets n
              propTester n propIsSet cases


propTester :: Int -> (Set Int -> Bool) -> [Set Int] -> IO ()
propTester n prop []        = print ("Passed all tests")
propTester n prop (c:cases) = do
                                if(prop c) then do
                                  print ("Passed on: " ++ (show c))
                                  propTester (n-1) prop cases
                                else
                                  print ("Failed on: " ++ (show c))

----------------
-- QuickCheck --
----------------
