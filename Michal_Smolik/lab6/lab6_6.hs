module Lab6_6 where

import Lecture6
import Lab6_5

testListMR :: Int -> [Integer] -> IO(Integer)
testListMR k [] = return 0
testListMR k (x:xs) = do
    v <- primeMR k x
    if v then return x else testListMR k xs

testMRcarmichael = testListMR 3 (take 100 carmichael)
{-
*Lab6_6> testMRcarmichael

This application has requested the Runtime to terminate it in an unusual way.
Please contact the application's support team for more information.

I wasn't able to find a carmichael number that would pass Miller-Rabin test
-}


isMersenne :: Integer -> IO(Bool)
isMersenne p = do
        if not (prime p) then return False else do
            b <- primeMR 3 ((2^p)-1)
            return b

mersennePrimes :: [Integer] -> IO([Integer])
mersennePrimes [] = do return []
mersennePrimes (x:xs) = do
        b <- isMersenne x
        ps <- mersennePrimes xs
        if b then return (((2^x)-1): ps)
            else return ps

mersenneNormal :: [Integer]
mersenneNormal = [2^p -1| p <- primes, prime (2^p -1)]
{-
*Lab6_6> mersennePrimes (take 9 primes)
[3,7,31,127,8191,131071,524287]
(0.85 secs, 57,357,696 bytes)

I wasn't able to find any larger mersenne primes, because for them the Miller-Rabin test crashes, even though I'm
still able to use `prime` function

mersenneNormal is list of mersenne primes, using `prime` function. It performs better than Miller-Rabin test, because it's
able to find one more prime without crashing 

*Lab6_6> mersenneNormal
[3,7,31,127,8191,131071,524287,2147483647

(this result is obtained after several minutes of running. Other Mersenne primes probably won't be found by this algorithm, since
the next one is (2^61)-1, which is too time and memory consuming to test.)
-}

