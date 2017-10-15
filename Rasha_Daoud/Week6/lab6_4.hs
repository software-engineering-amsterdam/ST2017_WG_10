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
        return $ nub $ sort (x:xs)

-- get the least composite number that fools the fermat tester algorithm.
leastComposite :: Int -> IO Integer
leastComposite k = do 
 xs <- breakIt k 20 -- depending on the n, we can change it to 100 or higher.
 return $ minimum xs

main64 = do
  print "with k = 1";
  breakIt 1 20; {-as you increase n, more numbers fool the algorithm -}
  leastComposite 1; 
  print "with k = 2";
  breakIt 2 20;
  leastComposite 2; 
  print "with k = 3";
  breakIt 3 20;
  leastComposite 3; 
  print "with k = 4";
  breakIt 4 20; 
  leastComposite 4; 
  
{- Executing the following will show that the least composite number that can fool the test was 9:

*Lab6_4> breakIt 1 20
[9,15,21,25,27,28,39,45,49,65,121]
(0.00 secs, 1,632,152 bytes)

*Lab6_4> breakIt 2 20
[9,15,21,75,91,133,225,341,481,561,595,703,861,1105,2501]
(0.07 secs, 62,207,288 bytes)

*Lab6_4> breakIt 3 20
[91,561,703,1105,1729,1891,2465,2821]
(0.36 secs, 285,970,776 bytes)

*Lab6_4> breakIt 4 20
[45,91,561,1105,1729,2465,2821,6601]
(0.87 secs, 456,246,024 bytes)

*Lab6_4> breakIt 5 20
[561,1105,1729,2465,2821,6601,10585]
(1.77 secs, 759,417,128 bytes)


If k > 3 then the execution time grows exponentially. We had to wait for quite some time to get result with k =6 or more

 -}