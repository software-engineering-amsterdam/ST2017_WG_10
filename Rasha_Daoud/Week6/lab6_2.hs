-- time: 40 minutes
module Lab6_2
where

import Data.List
import System.Random
import Lecture6
import Lab6_1

{- Check that your implementation is more efficient than expM by running a number of relevant tests and documenting the results. -}

-- we can generate a list of tuples (x,y,z) to test on each of them
genModList :: Int -> IO [(Integer,Integer,Integer)]
genModList 0 = return []
genModList n = do
 x <- getStdRandom (randomR (1,100000000))
 y <- getStdRandom (randomR (1,100000000))
 z <- getStdRandom (randomR (1,100000000))
 xs <- genModList (n-1)
 return ((x,y,z):xs)

-- let's test n times with the original function
randomTesterOrig [] = print "done"
randomTesterOrig ((x,y,z):xs) = do 
                let m = expM x y z
                randomTesterOrig xs

-- let's test n times with the new function
randomTesterNew [] = print "done"
randomTesterNew ((x,y,z):xs) = do 
                let m = exM' x y z
                randomTesterNew xs

-- random samples list
ys = genModList 100000

executeOrig :: IO ()
executeOrig = do
           putStrLn "Original function";
           xs <- ys;
           randomTesterOrig xs;

executeNew :: IO ()
executeNew = do
           putStrLn "New function";
           xs <- ys;
           randomTesterNew xs;

-- Now we can check whether both functions give the same result on the samples we generate
check [] = print "both functions give same results on the same list of samples...."
check ((x,y,z):xs)= do
           if (expM x y z /= exM' x y z) then
             error ("comparison failed....")
           else
            check xs

giveSameRes :: IO()
giveSameRes = do
   xs <- ys;
   check xs;

{- The conclusion is that, old implementation is a bit less efficient. Especially when taking big sample of data
and use big random integers in the tuple (e.g. generating numbers between 1 million and 1 billion, or 1 and 1 billion).

The test was done on 1 million samples, each sample contains random numbers generated between 1 and 10 million.

To set a timer, we used the following command in GHCi: :set +s

*Lab6_2> executeOrig
Original function
"done"
(4.79 secs, 4,592,908,528 bytes)
*Lab6_2> executeNew
New function
"done"
(4.70 secs, 4,592,900,776 bytes)


*Lab6_2> giveSameRes
"both functions give same results on the same list of samples...."

-}
