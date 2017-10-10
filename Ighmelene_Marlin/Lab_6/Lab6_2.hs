module Lab6_2 where

import Data.List
import System.Random
import Test.QuickCheck
import Data.String.Utils
import Lecture6

-- @ 300 min

main62 :: IO ()
main62 = do
          putStrLn ""
          putStrLn "Test correctness of new implementation"
          putStrLn "    testCorrectness 100 10000 0 -- Don't show test cases, just the end result"
          putStr   "    "
          testCorrectness 100 10000 0
          putStrLn ""
          putStrLn "Testing the old implementation"
          putStrLn "    testInEqClassOrig 100 100000 0 -- Don't show test cases, just the end result"
          putStr   "    "
          testInEqClassOrig 100 100000 0
          putStrLn ""
          putStrLn "Testing the new implementation"
          putStrLn "    testInEqClassNew 100 100000 0 -- Don't show test cases, just the end result"
          putStr   "    "
          testInEqClassNew 100 100000 0
          putStrLn ""

-----------
-- Tests --
-----------
{-
testCorrectness checks if the new implementation (exM) gives the same answers as the original implementation (expM).
In order to see when the difference becomes apparent, x, y and z are limited to various values.
Until 10.000 the 100 test cases run in very little time
  x, y, z are between 0 and 10.000
    *Lab6_2> testCorrectness 100 10000 1
    Passed: [874,2275,7153]
    ...
    Passed: [180,3143,5054]
    Passed all tests
    (0.05 secs, 7,976,408 bytes)
                        
At 100.000 the time starts increasing
  x, y, z are between 0 and 100.000
    *Lab6_2> testCorrectness 100 100000 1
    Passed: [83820,24477,63338]
    ...
    Passed: [68578,92897,37541]
    Passed all tests
    (0.56 secs, 32,855,552 bytes)

And it takes 7 - 8 seconds at 1.000.000
  x, y, z are between 0 and 1.000.000
    *Lab6_2> testCorrectness 100 1000000 1
    Passed: [171096,152407,412407]
    ...
    Passed: [932732,71680,328970]
    Passed all tests
    (7.74 secs, 320,984,616 bytes)

In order to see which implementation, if any, is causing the test to slow down, they're tested separately.

Testing only the new implementation using testInEqClassNew still returns very low times for both 100.000 and 1.000.000
  Run 100 tests on where x, y and z are between 0 and 100.000
    *Lab6_2> testInEqClassNew 100 100000 0
    Passed all tests
    (0.01 secs, 3,826,272 bytes)

  Run 100 tests on where x, y and z are between 0 and 1.000.000
    *Lab6_2> testInEqClassNew 100 1000000 0
    Passed all tests
    (0.02 secs, 4,331,168 bytes)

Testing only the original implementation using testInEqClassOrig returns times more similar to the ones returned when testing both implementations
-}
testCorrectness :: Int -> Int -> Int -> IO ()
testCorrectness n m v = do
                          cases <- genCases n m
                          propTester propEqual cases v

{-
Run 100 tests on where x, y and z are between 0 and 10
  *Lab6_2> testInEqClassNew 100 10 0
  Passed all tests
  (0.01 secs, 1,985,328 bytes)

Run 100 tests on where x, y and z are between 0 and 100
  *Lab6_2> testInEqClassNew 100 100 0
  Passed all tests
  (0.01 secs, 2,370,560 bytes)

Run 100 tests on where x, y and z are between 0 and 10.000
  *Lab6_2> testInEqClassNew 100 10000 0
  Passed all tests
  (0.01 secs, 3,390,728 bytes)

Run 100 tests on where x, y and z are between 0 and 100.000
  *Lab6_2> testInEqClassNew 100 100000 0
  Passed all tests
  (0.01 secs, 3,826,272 bytes)

Run 100 tests on where x, y and z are between 0 and 1.000.000
  *Lab6_2> testInEqClassNew 100 1000000 0
  Passed all tests
  (0.02 secs, 4,331,168 bytes)
-}
testInEqClassNew :: Int -> Int -> Int -> IO ()
testInEqClassNew n m v = do
                          cases <- genCases n m
                          propTester propInEqClassNew cases v

{-
Run 100 tests on where x, y and z are between 0 and 10
  *Lab6_2> testInEqClassOrig 100 10 0
  Passed all tests
  (0.01 secs, 1,827,912 bytes)

Run 100 tests on where x, y and z are between 0 and 100
  *Lab6_2> testInEqClassOrig 100 100 0
  Passed all tests
  (0.01 secs, 2,026,784 bytes)

Run 100 tests on where x, y and z are between 0 and 10.000
  *Lab6_2> testInEqClassOrig 100 10000 0
  Passed all tests
  (0.02 secs, 4,347,280 bytes)

Run 100 tests on where x, y and z are between 0 and 100.000
  *Lab6_2> testInEqClassOrig 100 100000 0
  Passed all tests
  (0.40 secs, 31,471,016 bytes)

Run 100 tests on where x, y and z are between 0 and 1.000.000
  *Lab6_2> testInEqClassOrig 100 1000000 0
  Passed all tests
  (7.07 secs, 332,323,200 bytes)
-}
testInEqClassOrig :: Int -> Int -> Int -> IO ()
testInEqClassOrig n m v = do
                          cases <- genCases n m
                          propTester propInEqClassOrig cases v

----------------
-- Properties --
----------------
propInEqClassNew :: Integer -> Integer -> Integer -> Bool
propInEqClassNew x y z = do
                          let m = exM x y z
                          m >= 0 && m < z

propInEqClassOrig :: Integer -> Integer -> Integer -> Bool
propInEqClassOrig x y z = do
                          let m = expM x y z
                          m >= 0 && m < z

propEqual :: Integer -> Integer -> Integer -> Bool
propEqual x y z = exM x y z == expM x y z

---------------
-- Interator --
---------------
propTester :: (Integer -> Integer -> Integer -> Bool) -> [(Integer,Integer,Integer)] -> Int -> IO ()
propTester prop [] _              = putStrLn ("Passed all tests")
propTester prop ((x,y,z):cases) v = do
                                      if(prop x y z) then do
                                        if(v == 1)
                                          then putStrLn ("Passed: " ++ show [x,y,z]) 
                                          else putStr ""
                                        propTester prop cases v
                                      else
                                        putStrLn ("Failed: " ++ show [x,y,z])

---------------
-- Generator --
---------------
genCases :: Int -> Int -> IO [(Integer,Integer,Integer)]
genCases n m = do
                xs <- getIntegers n m
                ys <- getIntegers n m
                ws <- getIntegers n m
                let zs = map succ ws
                let cases = [(xs!!i,ys!!i,zs!!i) | i <- [0..(n-1)]]
                return (cases)

getIntegers :: Int -> Int -> IO [Integer]
getIntegers 0 m = return []
getIntegers n m = do
                    y   <- getRandomInt m
                    let x = read $ show y :: Integer
                    xs  <- getIntegers (n-1) m
                    return (x:xs)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))
