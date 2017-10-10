-- time: 15 minutes
module Lab6_4
where

import Data.List
import System.Random

import Lecture6
import Lab6_1
import Lab6_2
import Lab6_3

{-Use the list of composite numbers to test Fermat's primality check. What is the least composite number that you can find that fools the check, for prime_tests_F k with k=1,2,3. What happens if you increase k?-}


testFermat _ [] = print "Fermat algorithm passed the test on composites numbers successfully..."
testFermat k (x:xs) = do 
 f <- primeTestsF k x
 print ("processing x = " ++ show x)
 if (f == False) then testFermat k xs
 else print ("fooling case with a composite number x = " ++ show x)

getFalseComposite :: Int -> [Integer] -> IO Integer
getFalseComposite k (x:xs) = do
    f <- primeTestsF k x
    if f == True then do return x else getFalseComposite k xs


breakIt :: Int -> Int -> IO [Integer] -- with n number of tests
breakIt _ 0 = return [] -- base case to stop recursion calls when n = 0
breakIt k n = do
        x <- getFalseComposite k composites'
        xs <- breakIt k (n-1)
        return $ sort (x:xs)

-- get the least composite number that fools the fermat tester algorithm.
leastComposite :: Int -> IO Integer
leastComposite k = do 
 xs <- breakIt k 20 -- depending on the n, we can change it to 100 or higher.
 return $ minimum xs

main64 = do
  print "with k = 1";
  breakIt 1 20;
  leastComposite 1; {- 9, 15 ... as you increase n, more numbers tries breaks the algorithm -}
  print "with k = 2";
  breakIt 2 20;
  leastComposite 2; {- 15, 65 ... -}
  print "with k = 3";
  breakIt 3 20;
  leastComposite 3; {- 15, 65, 49, 91  -}
  print "with k = 4";
  breakIt 4 20; 
  leastComposite 4; {- 9 ??	  1729 , 1105 , 8911 ...  -}
  
{- if k > 3 then the execution time grows exponentially. -}