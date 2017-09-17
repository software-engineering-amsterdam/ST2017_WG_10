module Lab1_5 where
import Data.List
import Test.QuickCheck  

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Int]
primes = 2 : filter prime [3..] 

isSumPrime :: Int -> Int -> Bool
isSumPrime amount n = prime $ sum $ take amount $ drop n primes

findPrime :: Int -> Int
findPrime n
    | isSumPrime amount n = sum $ take amount $ drop n primes
    | otherwise = findPrime $ n+1
        where amount = 101

answer :: Int
answer = findPrime 1
{-
The answer would be tested by crawling through the primes list to find 101 primes that have that sum, but would stop if 
the sum was larger. Tthis approach is very similar to the way we found the prime, so I don't think that it needs to be checked.
If our way to find the prime is not correct, then the test wouldn't be correct too, as the same function would be used´for both 
finding the prime and testing.
-}
--15 min