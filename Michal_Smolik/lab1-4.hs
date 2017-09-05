module Lab1_4 where
import Data.List
import Test.QuickCheck  

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..] 


--I call the primes, for which their reversal is also prime, reversable.

reversal :: Integer -> Integer
reversal = read . reverse . show

isReversePrime :: Integer -> Bool
isReversePrime n = prime n && prime (reversal n)

reversablePrimes :: [Integer]
reversablePrimes = filter isReversePrime primes

first_10000_reversable_primes :: [Integer]
first_10000_reversable_primes = take 10000 reversablePrimes
{-
testing the reversal could be done with another implementation of the same function, like arithmetically:
-}
arithmeticalReversal :: Integer -> Integer
arithmeticalReversal n = step n 0
    where
    step :: Integer -> Integer -> Integer
    step 0 a = a
    step x y = step (div x 10) (y*10+(mod x 10))
{-
then we test whether the two functions give the same answer. The test will be useless if they make same mistakes, but that should be
improbable at least.
-}
--10 min