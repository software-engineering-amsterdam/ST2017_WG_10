module Lab5_4

where 

import Data.List
import System.Random
import Lecture5

-- 30 min
{-
It's possible to generate problems with 3, 4 and 5 blank spots.
The larger the number of blank spots, the longer it takes to generate the problem.
Small test of the various problems (2 per type)
*Lab5_4> main54
+-------+-------+-------+
| 4 5 7 | 1 6 2 | 9 3 8 |
| 6 2 8 | 7 9 3 | 5 1 4 |
| 1 3 9 | 4 8 5 | 2 7 6 |
+-------+-------+-------+
| 8 1 5 | 2 7 9 | 6 4 3 |
| 2 7 4 | 6 3 8 | 1 9 5 |
| 9 6 3 | 5 4 1 | 7 8 2 |
+-------+-------+-------+
| 7 9 6 | 3 2 4 | 8 5 1 |
| 5 4 2 | 8 1 7 | 3 6 9 |
| 3 8 1 | 9 5 6 | 4 2 7 |
+-------+-------+-------+
 
+-------+-------+-------+
| 4 5 7 | 1 6 2 | 9 3 8 |
| 6 2 8 | 7 9 3 | 5 1 4 |
| 1 3 9 | 4 8 5 | 2 7 6 |
+-------+-------+-------+
| 8 1 5 | 2 7 9 | 6 4 3 |
| 2 7 4 |   3 8 | 1 9 5 |
| 9 6 3 | 5 4 1 | 7 8 2 |
+-------+-------+-------+
|   9 6 | 3 2 4 | 8 5 1 |
| 5 4 2 | 8 1 7 | 3 6 9 |
| 3 8 1 |   5 6 | 4 2 7 |
+-------+-------+-------+
Contains 3 blanks
 
+-------+-------+-------+
| 2 1 8 | 3 7 5 | 4 9 6 |
| 4 9 6 | 8 2 1 | 5 3 7 |
| 5 3 7 | 6 4 9 | 8 2 1 |
+-------+-------+-------+
| 8 6 1 | 7 9 4 | 3 5 2 |
| 9 4 3 | 5 6 2 | 7 1 8 |
| 7 2 5 | 1 3 8 | 9 6 4 |
+-------+-------+-------+
| 3 7 4 | 9 1 6 | 2 8 5 |
| 1 5 9 | 2 8 7 | 6 4 3 |
| 6 8 2 | 4 5 3 | 1 7 9 |
+-------+-------+-------+
 
+-------+-------+-------+
| 2 1 8 | 3 7 5 | 4 9 6 |
| 4 9   | 8 2 1 | 5 3 7 |
| 5 3 7 | 6 4 9 | 8 2 1 |
+-------+-------+-------+
| 8 6 1 | 7   4 | 3 5 2 |
| 9 4 3 | 5 6 2 | 7 1 8 |
| 7 2 5 | 1 3 8 | 9 6 4 |
+-------+-------+-------+
| 3 7 4 | 9 1 6 | 2 8 5 |
| 1 5 9 | 2   7 | 6 4 3 |
| 6 8 2 | 4 5 3 | 1 7 9 |
+-------+-------+-------+
Contains 3 blanks
 
Passed all tests
+-------+-------+-------+
| 6 5 2 | 1 9 3 | 8 7 4 |
| 7 4 3 | 6 8 5 | 9 2 1 |
| 9 1 8 | 7 4 2 | 3 6 5 |
+-------+-------+-------+
| 5 2 1 | 8 6 9 | 7 4 3 |
| 8 6 4 | 5 3 7 | 2 1 9 |
| 3 9 7 | 4 2 1 | 6 5 8 |
+-------+-------+-------+
| 4 7 9 | 2 5 8 | 1 3 6 |
| 1 3 5 | 9 7 6 | 4 8 2 |
| 2 8 6 | 3 1 4 | 5 9 7 |
+-------+-------+-------+
 
+-------+-------+-------+
| 6 5 2 | 1 9 3 | 8 7 4 |
| 7 4 3 | 6 8 5 | 9 2 1 |
| 9 1   | 7 4 2 | 3 6 5 |
+-------+-------+-------+
| 5 2 1 | 8 6 9 | 7 4 3 |
| 8 6 4 | 5 3 7 | 2 1 9 |
|   9   | 4 2 1 | 6 5 8 |
+-------+-------+-------+
| 4 7 9 | 2 5 8 | 1 3 6 |
| 1 3 5 | 9 7 6 | 4 8 2 |
| 2 8   | 3 1 4 | 5 9 7 |
+-------+-------+-------+
Contains 4 blanks
 
+-------+-------+-------+
| 7 1 9 | 4 2 3 | 5 6 8 |
| 4 8 3 | 1 6 5 | 2 7 9 |
| 5 6 2 | 7 9 8 | 4 3 1 |
+-------+-------+-------+
| 6 2 8 | 9 3 4 | 7 1 5 |
| 3 5 7 | 6 8 1 | 9 4 2 |
| 1 9 4 | 5 7 2 | 3 8 6 |
+-------+-------+-------+
| 8 7 1 | 2 4 9 | 6 5 3 |
| 2 4 5 | 3 1 6 | 8 9 7 |
| 9 3 6 | 8 5 7 | 1 2 4 |
+-------+-------+-------+
 
+-------+-------+-------+
| 7 1 9 | 4 2 3 | 5 6 8 |
| 4 8 3 | 1 6 5 | 2 7   |
| 5 6 2 | 7 9 8 | 4 3 1 |
+-------+-------+-------+
| 6 2 8 | 9 3 4 | 7 1 5 |
| 3 5 7 |   8   | 9 4 2 |
| 1 9 4 | 5 7 2 | 3 8 6 |
+-------+-------+-------+
| 8 7 1 | 2 4 9 | 6 5 3 |
| 2 4 5 | 3 1 6 | 8 9 7 |
| 9   6 | 8 5 7 | 1 2 4 |
+-------+-------+-------+
Contains 4 blanks
 
Passed all tests
+-------+-------+-------+
| 4 3 5 | 8 7 6 | 9 2 1 |
| 6 1 8 | 9 2 3 | 7 5 4 |
| 2 7 9 | 4 5 1 | 3 8 6 |
+-------+-------+-------+
| 8 4 7 | 1 3 5 | 6 9 2 |
| 5 2 3 | 6 8 9 | 4 1 7 |
| 9 6 1 | 7 4 2 | 8 3 5 |
+-------+-------+-------+
| 1 8 6 | 2 9 4 | 5 7 3 |
| 3 9 2 | 5 6 7 | 1 4 8 |
| 7 5 4 | 3 1 8 | 2 6 9 |
+-------+-------+-------+
 
+-------+-------+-------+
| 4 3 5 | 8 7 6 | 9 2 1 |
| 6 1 8 | 9 2 3 | 7 5 4 |
| 2 7 9 | 4 5 1 | 3 8   |
+-------+-------+-------+
| 8 4   | 1 3   | 6 9 2 |
| 5 2 3 | 6 8 9 | 4 1 7 |
| 9 6 1 |   4 2 | 8 3 5 |
+-------+-------+-------+
| 1 8 6 | 2 9 4 | 5 7 3 |
| 3 9 2 | 5 6 7 | 1 4   |
| 7 5 4 | 3 1 8 | 2 6 9 |
+-------+-------+-------+
Contains 5 blanks
 
+-------+-------+-------+
| 7 2 1 | 8 9 6 | 3 4 5 |
| 9 5 8 | 4 2 3 | 6 7 1 |
| 6 4 3 | 1 5 7 | 9 2 8 |
+-------+-------+-------+
| 2 8 7 | 3 4 9 | 1 5 6 |
| 5 9 4 | 6 7 1 | 8 3 2 |
| 3 1 6 | 2 8 5 | 7 9 4 |
+-------+-------+-------+
| 1 6 9 | 5 3 4 | 2 8 7 |
| 4 3 2 | 7 1 8 | 5 6 9 |
| 8 7 5 | 9 6 2 | 4 1 3 |
+-------+-------+-------+
 
+-------+-------+-------+
| 7 2 1 | 8 9 6 |   4 5 |
| 9 5 8 | 4 2 3 | 6 7 1 |
| 6 4 3 | 1 5 7 | 9 2 8 |
+-------+-------+-------+
| 2   7 | 3 4 9 | 1 5 6 |
| 5 9 4 | 6 7 1 | 8 3 2 |
| 3 1 6 | 2 8 5 | 7 9 4 |
+-------+-------+-------+
| 1 6   | 5 3 4 | 2 8 7 |
| 4 3 2 | 7 1   | 5 6 9 |
| 8 7 5 | 9 6 2 |   1 3 |
+-------+-------+-------+
Contains 5 blanks
 
Passed all tests
-}

main54 :: IO ()
main54 = do
            testContainsNBlanks 3 2
            testContainsNBlanks 4 2
            testContainsNBlanks 5 2

-- Adapted minimalize to stop after removing n valuesl.
-- Before removing another value, check if the number of openPositions is equal to n
minimalizeN :: Node -> [(Row,Column)] -> Int -> Node
minimalizeN node [] n = node
minimalizeN node ((r,c):rcs) n  | containsNBlanks node n = node -- Do not remove anymore values. Return Node
                                | uniqueSol node' = minimalizeN node' rcs n
                                | otherwise       = minimalizeN node  rcs n
                                where node' = eraseN node (r,c)

-- Generate a problem which contains n blanks
genProblemN :: Node -> Int -> IO Node
genProblemN node n = do 
                        ys <- randomize xs
                        return (minimalizeN node ys n)
                        where xs = filledPositions (fst node)

-- Test
testContainsNBlanks :: Int -> Int -> IO ()
testContainsNBlanks blanks n = do
                                probs <- genRandomProblems n blanks
                                let cases = [(s,p,blanks) | (s,p) <- probs]
                                probTester n containsNBlanks cases

-- Generator
genRandomProblems :: Int -> Int -> IO [(Node,Node)]
genRandomProblems n blanks  | n <= 0    = return []
                            | otherwise = do
                                            s     <- genRandomSudoku
                                            p     <- genProblemN s blanks
                                            probs <- genRandomProblems (n-1) blanks
                                            return ((s,p):probs)

-- Property
containsNBlanks :: Node -> Int-> Bool
containsNBlanks node n = length (openPositions (fst node))  == n

-- Test iterator
probTester :: Int -> (Node -> Int -> Bool) -> [(Node,Node,Int)] -> IO ()
probTester n prop []        = putStrLn ("Passed all tests")
probTester n prop ((s,p,b):cases) = do
                                    showNode s
                                    putStrLn " "
                                    showNode p
                                    if(prop p b) then do
                                      putStrLn ("Contains " ++ (show b) ++ " blanks")
                                      putStrLn " "
                                      probTester (n-1) prop cases
                                    else
                                      putStrLn ("Does NOT contain " ++ (show b) ++ " blanks")
