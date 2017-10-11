module Lab6_4 where

import Data.List
import System.Random
import Lecture6

-- 180 min
main64 :: IO ()
main64 = doMain64 0

{-
In order to know which composites are false primes we can call falseFermatPrimes:

    *Lab6_4> falseFermatPrimes 
    [9,15,21,25,27,28,33,35,39,45,49,51,52,55,57,63,65,66,69,70,75,76,77,81,85,87,91,93,95,99,105,111,...]


Or getFalseFPrimePositives to get n false primes:

    *Lab6_4> getFalseFermatPrimes 50
    [9,15,21,25,27,28,33,35,39,45,49,51,52,55,57,63,65,66,69,70,75,76,77,81,85,87,91,93,95,99,105,111,112,115,117,119,121,123,124,125,129,130,133,135,141,143,145,147,148,153]
    (0.01 secs, 195,616 bytes)

For now let's focus on just 1 false prime (9) and see if primeTestsF will expose it:
For k=1, 9 was exposed 15 times
For k=2, 9 was exposed 3 times
For k=3, 9 was exposed 1 time
For k=4, 9 was exposed 0 times

      *Lab6_4> iterateTestFermatsAlgorithms 100 1 (getFalseFermatPrimes 1) 0
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9
      Probably prime (k = 1):  9

      (0.01 secs, 967,016 bytes)

      *Lab6_4> iterateTestFermatsAlgorithms 100 2 (getFalseFermatPrimes 1) 0
      Probably prime (k = 2):  9
      Probably prime (k = 2):  9
      Probably prime (k = 2):  9

      (0.01 secs, 818,736 bytes)

      *Lab6_4> iterateTestFermatsAlgorithms 100 3 (getFalseFermatPrimes 1) 0
      Probably prime (k = 3):  9

      (0.01 secs, 869,520 bytes)

      *Lab6_4> iterateTestFermatsAlgorithms 100 4 (getFalseFermatPrimes 1) 0

      (0.01 secs, 947,144 bytes)

Increasing the number of false primes returns similar results; less false positives as k increases

    *Lab6_4> main64 
    -- let composites = getFalseFermatPrimes 4
    -- iterateTestFermatsAlgorithms 100 1 composites verbose
    -- iterateTestFermatsAlgorithms 100 2 composites verbose
    -- iterateTestFermatsAlgorithms 100 3 composites verbose
    -- iterateTestFermatsAlgorithms 100 4 composites verbose
    Probably prime (k = 1):  15
    Probably prime (k = 1):  15
    Probably prime (k = 1):  15
    Probably prime (k = 1):  21
    Probably prime (k = 1):  21
    Probably prime (k = 1):  9
    Probably prime (k = 1):  25
    Probably prime (k = 1):  15
    Probably prime (k = 1):  21
    Probably prime (k = 1):  9
    Probably prime (k = 1):  21
    Probably prime (k = 1):  25
    Probably prime (k = 1):  21
    Probably prime (k = 1):  9
    Probably prime (k = 1):  25
    Probably prime (k = 1):  21
    Probably prime (k = 1):  15
    Probably prime (k = 1):  15
    Probably prime (k = 1):  9
    Probably prime (k = 1):  21
    Probably prime (k = 1):  15
    Probably prime (k = 1):  15
    Probably prime (k = 1):  15
    Probably prime (k = 1):  9
    Probably prime (k = 1):  9
    Probably prime (k = 1):  15
    Probably prime (k = 1):  15
    Probably prime (k = 1):  9
    Probably prime (k = 1):  9
    Probably prime (k = 1):  21
    Probably prime (k = 1):  21
    Probably prime (k = 1):  25
    Probably prime (k = 1):  25
    Probably prime (k = 1):  21
    Probably prime (k = 1):  25
    Probably prime (k = 1):  21
    Probably prime (k = 1):  21
    Probably prime (k = 1):  21
    Probably prime (k = 1):  21
    Probably prime (k = 1):  15
    Probably prime (k = 1):  15
    Probably prime (k = 1):  21
    Probably prime (k = 1):  9
    Probably prime (k = 1):  15
    Probably prime (k = 1):  15
    Probably prime (k = 1):  15
    Probably prime (k = 1):  9
    Probably prime (k = 1):  15
    Probably prime (k = 1):  9
    Probably prime (k = 1):  15
    Probably prime (k = 1):  25
    Probably prime (k = 1):  15
    Probably prime (k = 1):  25
    Probably prime (k = 1):  21
    Probably prime (k = 1):  9
    Probably prime (k = 1):  25
    Probably prime (k = 1):  15
    Probably prime (k = 1):  21

    Probably prime (k = 2):  9
    Probably prime (k = 2):  15
    Probably prime (k = 2):  15
    Probably prime (k = 2):  15
    Probably prime (k = 2):  21
    Probably prime (k = 2):  9
    Probably prime (k = 2):  15
    Probably prime (k = 2):  25
    Probably prime (k = 2):  9
    Probably prime (k = 2):  21
    Probably prime (k = 2):  21

    Probably prime (k = 3):  21
    Probably prime (k = 3):  15

    -- Nothing for k = 4
    (0.05 secs, 13,250,784 bytes)

Now let's look at all composites.
The test always returns a false positive but as k increases so do the values
For k=1 the composites are between 25 and 91
For k=2 the composites are between 91 and 1387
For k=3 the composites are between 91 and 12403
For k=4 the composites are between 561 and 22321
Of course this is just 1 run with 10 cases for each k, but multiple runs return similar values.
The number of divisors that can cause p to appear as a Fermat prime differs per number.
And when k increases and more numbers are generated, the chances of finding only numbers which expose p as a Fermat prime decreases.
But because the larger numbers probably have more divisors (and more a's which return a false prime) they can pretend to be a prime longer (higher k)

  *Lab6_4> main64 
  -- iterateTestFermatsAlgorithms 10 1 composites verbose
  -- iterateTestFermatsAlgorithms 10 2 composites verbose
  -- iterateTestFermatsAlgorithms 10 3 composites verbose
  -- iterateTestFermatsAlgorithms 10 4 composites verbose
  Probably prime (k = 1):  35
  Probably prime (k = 1):  25
  Probably prime (k = 1):  65
  Probably prime (k = 1):  45
  Probably prime (k = 1):  85
  Probably prime (k = 1):  25
  Probably prime (k = 1):  91
  Probably prime (k = 1):  27
  Probably prime (k = 1):  63
  Probably prime (k = 1):  85

  Probably prime (k = 2):  91
  Probably prime (k = 2):  1387
  Probably prime (k = 2):  231
  Probably prime (k = 2):  561
  Probably prime (k = 2):  703
  Probably prime (k = 2):  1105
  Probably prime (k = 2):  1105
  Probably prime (k = 2):  561
  Probably prime (k = 2):  301
  Probably prime (k = 2):  1105

  Probably prime (k = 3):  1105
  Probably prime (k = 3):  6601
  Probably prime (k = 3):  1105
  Probably prime (k = 3):  561
  Probably prime (k = 3):  1105
  Probably prime (k = 3):  12403
  Probably prime (k = 3):  91
  Probably prime (k = 3):  1105
  Probably prime (k = 3):  561
  Probably prime (k = 3):  1105

  Probably prime (k = 4):  22321
  Probably prime (k = 4):  2465
  Probably prime (k = 4):  2821
  Probably prime (k = 4):  2465
  Probably prime (k = 4):  2465
  Probably prime (k = 4):  1729
  Probably prime (k = 4):  1105
  Probably prime (k = 4):  561
  Probably prime (k = 4):  2821
  Probably prime (k = 4):  15841

  (2.55 secs, 1,229,367,256 bytes)
-}

---------------
-- Iterators --
---------------
iterateTestFermatsAlgorithms :: Int -> Int -> [Integer] -> Int -> IO ()
iterateTestFermatsAlgorithms 0 _ _ _        = putStrLn ""
iterateTestFermatsAlgorithms n k xs verbose = do
                                                if (verbose == 1) then putStr ((show n) ++ " ") else putStr ""
                                                a <- testFermatsAlgorithmsCount n k xs verbose
                                                iterateTestFermatsAlgorithms (n-1) k xs verbose

testFermatsAlgorithmsCount :: Int -> Int -> [Integer] -> Int -> IO ()
testFermatsAlgorithmsCount _ _ [] v = if(v == 1) then putStrLn ("Composite") else putStr ""
testFermatsAlgorithmsCount n k (x:xs) v = do
                                              p <- primeTestsF k x
                                              if p then
                                                putStrLn ("Probably prime (k = "++(show k)++"):  " ++ (show x)) 
                                              else do
                                                testFermatsAlgorithmsCount n k xs v

-------------
-- Helpers --
-------------
-- If verbose, only print cases which return "Probably prime" otherwise print all test cases
doMain64 :: Int -> IO ()
doMain64 verbose = do
  let n = 100
  iterateTestFermatsAlgorithms n 1 composites verbose
  iterateTestFermatsAlgorithms n 2 composites verbose
  iterateTestFermatsAlgorithms n 3 composites verbose
  iterateTestFermatsAlgorithms n 4 composites verbose

-- Print all test cases
verboseMain64 :: IO ()
verboseMain64 = doMain64 1

-- Get (infinite) list of composites which can appear to be Fermat primes
falseFermatPrimes :: [Integer]
falseFermatPrimes  = do
                      let falseFP  = map (\c -> (c,getFalseFPrimePositives c)) composites
                      [p | (p,xs) <- falseFP, length xs > 0]

-- Get the first n composites which can appear to be Fermat primes
getFalseFermatPrimes :: Int -> [Integer]
getFalseFermatPrimes n  = take n falseFermatPrimes

-- For an integer, return the integers which would make p appear to be a Fermat prime
getFalseFPrimePositives :: Integer -> [Integer]
getFalseFPrimePositives p
  | prime p   = []
  | otherwise = do
                  let le = takeWhile (<= p) composites
                  if(length le > 0) then 
                    filter (\x -> mod (x^(p-1)) p == 1) (init le)
                  else []


