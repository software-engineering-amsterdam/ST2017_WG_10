-- Red Curry  - time 40
module Exercise1 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
--Useful logic notation
--infix 1 --
--(-->) :: Bool -Bool -Bool
--p --q = (not p) || q

--Your programmer Red Curry has written the following function for generating lists of floating point numbers.
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

{- 
	He claims that these numbers are random in the open interval 
	(0..1)
	. Your task is to test whether this claim is correct, by counting the numbers in the quartiles
	(0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1)
	and checking whether the proportions between these are as expected.
	E.g., if you generate 10000 numbers, then roughly 2500 of them should be in each quartile.
	Implement this test, and report on the test results.
-}


-- First, we need a function to determine if all generated numbers are between 0.0 & 1.0 (min & max)
withinRange :: Float -> Float -> [Float] -> Bool
withinRange min max xs = and [x >min && x <max| x <- xs]

{- Let us allow for a mean error of 100 in the evenly distribution
(because among the 10000, the distribution is roughl 2500 in each of the given range) -}
mean :: Int
mean = 100

-- Then, we need a function (property) to check whether the distribution of numbers is roughly equal (accept mean error)
{- my post conditions -}
isDistributedEvenly :: [Float] -> IO Bool
isDistributedEvenly xs = do
							m <- getDistribution
							if (withinRange 0.0 1.0 xs)
							&&  (abs ((m !! 0) - (m !! 1)) <= mean)
							&&  (abs ((m !! 1) - (m !! 2)) <= mean)
						    &&  (abs ((m !! 2) - (m !! 3)) <= mean)
							then return True
						    else return False
						     
{--------------------------------------------Testing---------------------------------------------------------}
-- we need a function to return the distribution (four quarters)
getDistribution :: IO [Int]
getDistribution = do
					  a <- probs 10000
					  let p1 = length (filter (< 0.25) a)
					  let p2 = length (filter (\b -> (b < 0.5)&&(b >= 0.25)) a)
					  let p3 = length (filter (\b -> (b < 0.75)&&(b >= 0.5)) a)
					  let p4 = length (filter (>= 0.75) a)
					  return (p1:p2:p3:p4:[])

main = do
 getDistribution

{- GHCi:
       *Exercise1> main
       [2462,2470,2560,2508]
-}

testIter :: Int -> Int -> ([Float] -> Bool) -> IO ()
testIter k n r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- probs 10000 -- test the programmer's function with 10000 generated numbers
                  if r xs then
                    do print ("pass one test on:" ++ show k)
                       testIter (k+1) n r
                  else error ("failed test on:" ++ show k)

testProbs :: ([Float] -> Bool) -> IO ()
testProbs p = testIter 1 100 p
-- GHCi: testProbs isDistributedEvenly