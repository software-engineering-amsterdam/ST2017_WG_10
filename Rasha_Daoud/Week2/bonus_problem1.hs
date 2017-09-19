-- projecteuler problem1 - time: 5 mins
module Problem1 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we

	get 3, 5, 6 and 9. The sum of these multiples is 23.


	Find the sum of all the multiples of 3 or 5 below 1000.
--}

sumMultBelowLimit :: Integer -> Integer
sumMultBelowLimit n = sum [a | a <- [1..(n-1)], (mod a 3 == 0) || (mod a 5 == 0)]

{- GHCi:
	*Problem1> sumMultBelowLimit 1000
	233168
-}