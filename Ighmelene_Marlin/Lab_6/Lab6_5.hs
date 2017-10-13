module Lab6_5 where

import Data.List
import System.Random
import Lecture6

-- 60 min

{-
The low values for k used in the previous exercise don't expose many Carmichael numbers as composites.
But higher k values do expose more Carmichael numbers as composites.

*Lab6_5> main65 
Looking for false primes

Each dot (.) represents an exposed composite

Probably prime (k = 1):  294409


Probably prime (k = 2):  294409


Probably prime (k = 4):  294409


Probably prime (k = 8):  294409

.
Probably prime (k = 16):  56052361

.
Probably prime (k = 32):  56052361

..
Probably prime (k = 64):  118901521

..
Probably prime (k = 128):  118901521

.........
Probably prime (k = 256):  11346205609

.......
Probably prime (k = 512):  2301745249

.............
Probably prime (k = 1024):  65700513721

...............
Probably prime (k = 2048):  100264053529

...............
Probably prime (k = 4096):  100264053529

..........................
Probably prime (k = 8192):  1396066334401

.............................................................
Probably prime (k = 16384):  22027380041449

.................................................
Probably prime (k = 32768):  11004252611041

.............................................................................................................
Probably prime (k = 65536):  381144706349401

(160.48 secs, 79,444,708,016 bytes)
-}

main65 :: IO ()
main65 = doMain65 0

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

---------------
-- Iterators --
---------------
iterateTestFermatsAlgorithms :: Int -> Int -> [Integer] -> Int -> IO ()
iterateTestFermatsAlgorithms 0 _ _ _        = putStrLn ""
iterateTestFermatsAlgorithms n k xs verbose = do
                                                a <- testFermatsAlgorithmsCount n k xs verbose
                                                iterateTestFermatsAlgorithms (n-1) k xs verbose

testFermatsAlgorithmsCount :: Int -> Int -> [Integer] -> Int -> IO ()
testFermatsAlgorithmsCount _ _ [] v = putStr ""
testFermatsAlgorithmsCount n k (x:xs) v = do
                                              p <- primeTestsF k x
                                              if p then do
                                                putStrLn ""
                                                putStrLn ("Probably prime (k = "++(show k)++"):  " ++ (show x)) 
                                              else do
                                                if (v == 1) then putStrLn ("Composite: " ++ (show x)) else putStr "."
                                                testFermatsAlgorithmsCount n k xs v

-------------
-- Helpers --
-------------
-- If verbose, only print cases which return "Probably prime" otherwise print all test cases
doMain65 :: Int -> IO ()
doMain65 verbose = do
  let n = 1
  putStrLn "Looking for false primes"
  if(verbose == 0) then
    putStrLn "\nEach dot (.) represents an exposed composite"
  else
    putStr ""
    
  iterateTestFermatsAlgorithms n 1 carmichael verbose
  iterateTestFermatsAlgorithms n 2 carmichael verbose
  iterateTestFermatsAlgorithms n 4 carmichael verbose
  iterateTestFermatsAlgorithms n 8 carmichael verbose
  iterateTestFermatsAlgorithms n 16 carmichael verbose
  iterateTestFermatsAlgorithms n 32 carmichael verbose
  iterateTestFermatsAlgorithms n 64 carmichael verbose
  iterateTestFermatsAlgorithms n 128 carmichael verbose
  iterateTestFermatsAlgorithms n 256 carmichael verbose
  iterateTestFermatsAlgorithms n 512 carmichael verbose
  iterateTestFermatsAlgorithms n 1024 carmichael verbose
  iterateTestFermatsAlgorithms n 2048 carmichael verbose
  iterateTestFermatsAlgorithms n 4096 carmichael verbose
  iterateTestFermatsAlgorithms n 8192 carmichael verbose
  iterateTestFermatsAlgorithms n 16384 carmichael verbose
  iterateTestFermatsAlgorithms n 32768 carmichael verbose
  iterateTestFermatsAlgorithms n 65536 carmichael verbose
  
-- Print all test cases
verboseMain65 :: IO ()
verboseMain65 = doMain65 1

