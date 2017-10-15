-- time: 45 minutes
module Lab6_5
where

import Data.List
import System.Random
import Lecture6
import Lab6_1
import Lab6_2
import Lab6_3
import Lab6_4

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

-- we can adjust the test function from the previous exercise to use carmichael list instead of composites'
breakIt' :: Int -> Int -> IO [Integer] -- with n number of tests
breakIt' _ 0 = return [] -- base case to stop recursion calls when n = 0
breakIt' k n = do
        x <- getFalseComposite k carmichael
        xs <- breakIt' k (n-1)
        return $ sort (x:xs)

{- this case breaks the algorithm all the time, among other [294409] -}

-- Let's define a function to test n times on different k 

carmichaelTestIter :: Int -> Int -> [Integer] -> IO Bool
carmichaelTestIter 0 _ _ =  return True -- base case
carmichaelTestIter n k (x:xs) = do
   checkResult <- primeTestsF k x
   if(checkResult) then do
       print ("problem in case x = " ++ show x)
       return False
   else do
        carmichaelTestIter (n-1) k xs

-- the random tester...
testN :: Int -> Int -> IO Bool
testN n k = carmichaelTestIter n k carmichael

main65 = do
  print "with k = 1";
  testN 100 1;
  print "with k = 2";
  testN 100 2;
  print "with k = 3"; 
  testN 100 3;
  print "with k = 4"; 
  testN 100 4;

{- 

*Lab6_5> main65
"with k = 1"
"problem in case x = 294409"
"with k = 2"
"problem in case x = 294409"
"with k = 3"
"problem in case x = 294409"
"with k = 4"
"problem in case x = 294409"
False
(0.42 secs, 17,045,320 bytes)

if we try with a bigger k, while k is still <=10, the test already fails always on the first number in  the carmichael list.

While trying k > 10 (e.g. 11), the test passes few numbers before it fails again:
*Lab6_5> testFermat 11 carmichael
"processing x = 294409"
"processing x = 56052361"
....


Conclusion: seems like the list of carmichael should not be used to test fermat. The likelyhood that the test fails is really high (always fails with k <=10).
-}



tryBreaking = do  
  print "with k = 1";
  testFermat 1 carmichael; -- always failing
  print "with k = 2";
  testFermat 2 carmichael; -- always failing
  print "with k = 3"; 
  testFermat 3 carmichael; -- always failing

{-
*Lab6_5> tryBreaking
"with k = 1"
"processing x = 294409"
"fooling case with a composite number x = 294409"
"with k = 2"
"processing x = 294409"
"fooling case with a composite number x = 294409"
"with k = 3"
"processing x = 294409"
"fooling case with a composite number x = 294409"
(0.19 secs, 12,095,848 bytes)

-}


