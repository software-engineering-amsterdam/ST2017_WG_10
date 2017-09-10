-- Summation of primes  -time: 10 mins
import Data.List
import Test.QuickCheck

-- The following two functions are given in previous assignments
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
           where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

{-  https://projecteuler.net/problem=10
    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
	Find the sum of all the primes below two million.
-}

-- Wrtie a function to get the list of all primes below an integer value (e.g. 2000000)
primesBelowLimit :: Integer -> [Integer]
primesBelowLimit l = [x | x <- filter prime [2..(l-1)]] -- below l

sumPrimesBelow2000000 = sum (primesBelowLimit 2000000)

{- GHCi: 
  *Main> sumPrimesBelow2000000
  142913828922
-}





{- We've chosen this solution, because only one member solved problem10 -}