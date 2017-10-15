-- time: 15 minutes
module Lab6_6
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

{- Use the list from the previous exercise to test the Miller-Rabin primality check. What do you find? -}

-- we can use the same implementation from lab6_5, and adjust the function we calll to MR.

carmichaelTestIterMR :: Int -> Int -> [Integer] -> IO Bool
carmichaelTestIterMR 0 _ _ =  return True -- base case
carmichaelTestIterMR n k (x:xs) = do
   checkResult <- primeMR k x
   if(checkResult) then do
      return False
   else do
        carmichaelTestIterMR (n-1) k xs -- n is number of test-iterations

-- the random tester...
testMR :: Int -> Int -> IO Bool
testMR n k = carmichaelTestIterMR n k carmichael

-- repeat test n = 1 (one time)
main66 = do
  print "MR with k = 1";
  testMR 1 1;
  print "MR with k = 2";
  testMR 1 2;
  print "MR with k = 3"; 
  testMR 1 3;
  print "MR with k = 4"; 
  testMR 1 4;


{- 
applying the Miller-Rabin algorithmon camichael list, produces no false positives

*Lab6_6> main66
"MR with k = 1"
"MR with k = 2"
"MR with k = 3"
"MR with k = 4"
True

try to increase n, and repeat the test 2, 3, 10 times. The result will stay the same, however the run-time will be bigger.

-}