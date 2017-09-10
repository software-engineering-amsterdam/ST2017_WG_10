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

{-

The answer would be tested by crawling through the primes list to find 101 primes that have that sum, but would stop if 

the sum was larger. Tthis approach is very similar to the way we found the prime, so I don't think that it needs to be checked.

If our way to find the prime is not correct, then the test wouldn't be correct too, as the same function would be used´for both 

finding the prime and testing.

-}

--15 min

{- We've combined two answers. Took best solution & best testing argument -}