module Lab1 where
import Data.List
import Data.Numbers.Primes
import Test.QuickCheck

-- 50 min
-- I would say I don't have to test it, but maybe because I wouldn't know what I'm testing
getNPrimesStartingAt :: Int -> Int -> [Int]
getNPrimesStartingAt n m = take n ([x | x <- primes, x > m])

getLowestSum :: Int -> Int
getLowestSum n =  if(isPrime total)
                  then total 
                  else getLowestSum (n+1)
                    where total = sum (getNPrimesStartingAt 101 n)