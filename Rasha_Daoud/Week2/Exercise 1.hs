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


quartL :: [Float] -> Float
quartL xs = fromIntegral(length xs) / 4

-- Second, check the resulting list has any duplicates
hasDuplicates :: [Float] -> Bool
hasDuplicates []  = False
hasDuplicates [_] = False
hasDuplicates (x:xs) | elem x xs = True
                     | otherwise = hasDuplicates xs

-- Let us allow for a mean of 0.05 (because among the 10000, the distribution is roughl 2500 in each of the given range)
mean :: Float
mean = 0.05

calculateFreq :: Float -> Float -> [Float] -> Float
calculateFreq min max xs = fromIntegral (length (filter (>min) (filter (<=max) xs))) / fromIntegral (length xs)

-- Then, we need a function to check whether the distribution of numbers is roughly equal (accept 0.05)
{- my post conditions -}
isDistributedEvenly :: [Float] -> Bool
isDistributedEvenly xs =
                          withinRange 0.0 1.0 xs
                        --  && not(hasDuplicates xs)
						  && (calculateFreq 0.0 0.25 xs)  == quartL xs + mean
						  && (calculateFreq 0.25 0.5 xs)  == quartL xs + mean
						  && (calculateFreq 0.5 0.75 xs)  == quartL xs + mean
						  && (calculateFreq 0.75 1.0 xs)  == quartL xs + mean

testIter :: Int -> Int -> ([Float] -> Bool) -> IO ()
testIter k n r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- probs 10000
                  if r xs then
                    do print ("pass one test")
                       testIter (k+1) n r
                  else error ("failed test on:" ++ show k)

testProbs :: ([Float] -> Bool) -> IO ()
testProbs p = testIter 1 100 p
-- testProbs isDistributedEvenly