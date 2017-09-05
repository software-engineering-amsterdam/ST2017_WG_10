module Lab1_6 where
import Data.List
import Test.QuickCheck  

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Int]
primes = 2 : filter prime [3..] 

{-
searchIndex(current_index, prime) finds us the index of a prime in the `primes` list.
current_index is used like a local variable, should be initially called with `current_index = 0`
-}
searchIndex :: Int -> Int -> Int
searchIndex p i
    | (head(drop i primes)) == p = i+1
    | (head(drop i primes)) > p = -1
    | otherwise = searchIndex p $i+1

--checks if (p_1*p_2*...*n)+1 is prime. n is the prime, not its index.
isProductPrime :: Int -> Bool
isProductPrime n =  not . prime $ (foldr (*) 1 $ take (searchIndex n 0) primes) + 1

wrongPrimes :: [Int]
wrongPrimes = filter isProductPrime primes

{-
computes (p_1*p_2*...*n)+1 for each n, for which the result is not prime. These are counterexamples.
their primes can be found in `wrongPrimes`
-}
counterexamples :: [Int]
counterexamples = map (\n -> (foldr (*) 1 $ take (searchIndex n 0) primes) + 1) wrongPrimes

lowest_counterexample :: Int
lowest_counterexample = head counterexamples
--15 min
