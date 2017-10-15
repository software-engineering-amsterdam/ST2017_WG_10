-- time: 
-- marcenne numbers 
module Lab6_7
where

import Data.List
import System.Random
import Control.Exception
import Data.Time
import Lecture6
import Lab6_1
import Lab6_2
import Lab6_3
import Lab6_4
import Lab6_5
import Lab6_6

{- You can use the Miller-Rabin primality check to discover some large Mersenne primes. Find information about Mersenne primes on internet and check whether the numbers that you found are genuine Mersenne primes. Report on your findings.
-}

-- first we need a checker whether a number is a marsenne prime
isMersennePrime :: Integer -> Bool
isMersennePrime n = prime n && prime (2^(n-1) -1)


marsenneTestIter:: Int -> Int -> [Integer] -> IO Bool
marsenneTestIter 0 _ _ =  return True -- base case
marsenneTestIter n k (x:xs) = do
   checkResult <- primeMR k x
   if(checkResult) then do
      marsenneTestIter (n-1) k xs -- n is number of test-iterations
   else do return False
        

-- the random tester...
testMillerRabinOnMarsenneTest :: Int -> Int -> IO Bool
testMillerRabinOnMarsenneTest n k = marsenneTestIter n k marsennePrimes
                                    where marsennePrimes =  map mers [1..100] -- generate marsenne primes up to mers 100 from lecture 6

-- repeat test n = 1 (one time)
main67 = do
  print "Miller-Rabin with marsenne primes and k = 1";
  testMillerRabinOnMarsenneTest 1 1;
  print "Miller-Rabin with marsenne primes and k = 2";
  testMillerRabinOnMarsenneTest 1 2;
  print "Miller-Rabin with marsenne primes and = 3";
  testMillerRabinOnMarsenneTest 1 3;
  print "Miller-Rabin with marsenne primes and k = 4";
  testMillerRabinOnMarsenneTest 1 4;
  print "Miller-Rabin with marsenne primes and k = 1000";
  testMillerRabinOnMarsenneTest 1 1000;
  print "Miller-Rabin with marsenne primes and k = 2000";
  testMillerRabinOnMarsenneTest 1 2000;  
  --- you can try with any k >= 1

{-

*Lab6_7> main67
"Miller-Rabin with marsenne primes and k = 1"
"Miller-Rabin with marsenne primes and k = 2"
"Miller-Rabin with marsenne primes and = 3"
"Miller-Rabin with marsenne primes and k = 4"
"Miller-Rabin with marsenne primes and k = 1000"
"Miller-Rabin with marsenne primes and k = 2000"
True
(0.04 secs, 17,147,744 bytes)

As you can see, all marsenne primes up to (2^(100-1) -1) passed M-R tests. 
-}