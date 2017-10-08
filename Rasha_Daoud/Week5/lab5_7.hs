--time: 20 minutes
module Lab5_7
where

import Data.List
import System.Random
import Lecture5

{- Minimal problems for NRC Sudokus need fewer hints than standard Sudoku problems. Investigate the difference. What is the average number of hints in a minimal standard Sudoku problem? What is the average number of hints in a minimal NRC Sudoku problem? -}


{- let's use a similar function as in lecture2.hs -}
averageFilledPos :: Int -> Int -> Int -> IO ()
averageFilledPos k n var = if k == n then
                             print (fromIntegral var / fromIntegral n)
                           else do
                                s <- genRandomSudoku
                                p <- genProblem s
                                averageFilledPos (k+1) n (var + (length $ filledPositions $ fst p))
								-- until counter k = n (number of iterations)

{- 
let's take 20 problems and calculate the average:

GHCi:
*Lab5_7> averageFilledPos 0 20 0
24.1

-}