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
       print ("Miller Rabin -- problem in case x = " ++ show x)
       return False
   else do
        carmichaelTestIter (n-1) k xs

-- the random tester...
testMR :: Int -> Int -> IO Bool
testMR n k = carmichaelTestIterMR n k carmichael

main66 = do
  print "MR with k = 1";
  testMR 100 1;
  print "MR with k = 2";
  testMR 100 2;
  print "MR with k = 3"; 
  testMR 100 3;
  print "MR with k = 4"; 
  testMR 100 4;


{- 
*Lab6_6> main66
"MR with k = 1"
"Millar Rabin -- problem in case x = 294409"
"MR with k = 2"
"problem in case x = 56052361"
"MR with k = 3"
"problem in case x = 56052361"
"MR with k = 4"
"problem in case x = 56052361"
False
(168.91 secs, 4,315,105,960 bytes)

-}