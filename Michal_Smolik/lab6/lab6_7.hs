module Lab6_7 where
import Lecture6

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

*Lab6_7> primeMR 1 (2^29 -1)

This application has requested the Runtime to terminate it in an unusual way.
Please contact the application's support team for more information.
GNU MP: Cannot allocate memory (size=4228907032)



I wasn't able to find any larger Mersenne primes with this function, because for larger exponents the Miller-Rabin test crashes,
even if we test them just once. I'm still able to use the `prime` function to test primality though. 

mersenneNormal is list of Mersenne primes, using the `prime` function to test primality. It obviously performs better than Miller-Rabin test,
since it's able to find one more Mersenne prime without crashing 

*Lab6_6> mersenneNormal
[3,7,31,127,8191,131071,524287,2147483647

(this result is obtained after several minutes of running. Other Mersenne primes probably won't be found by this algorithm, since
the next one is (2^61)-1, which is too time and memory consuming to test.)
-}
