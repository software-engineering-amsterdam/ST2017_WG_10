-- Exercise 5 -time: 20 minutes
module Exercise5 where
import Data.List
import Test.QuickCheck 

-- The following two functions are given in the assignment
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
           where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..] 

-- Find the smallest prime number that is a sum of 101 consecutive primes.
-- I made the function generalized and didn't hardcode 101
consPrimes :: Int -> [Integer] -> Integer
consPrimes n primesList | prime (sum (take n primesList)) = sum (take n primesList)
                          -- we found first one, because of lazy evaluation the second case won't be executed
                        | otherwise = consPrimes n (tail primesList)
				        -- we didn't find the minimun yet, go fetch the rest of the primesList (using tail)

{-- Test in GHCi
	consPrimes 101 primes
	37447
--}

{-
  - Do you have to test that your answer is correct? How could this be checked
  
	A: if we're going to test it with n = 101, you still have to make sure to give the function a list
	that contains at least 101 consecutive primes, otherwise the function won't find the value
    In my execution I used 'primes' which produces an infinte list, then I called my function on 101 and that list,
    and I know the list contains the prime numbers I need to adds up to a prime 37447, take & tail are safe enough (lazy evaluation).
	
    The problem might occur when the list doesn't fulfil that. The function might not terminate! 
-}

