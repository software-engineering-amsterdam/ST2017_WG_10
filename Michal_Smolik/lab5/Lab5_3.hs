
module Lab5_3

where 

import Data.List
import System.Random
import Lecture5

hasSingleSolution :: Sudoku -> Bool
hasSingleSolution s = 1 == length(solveNs(initNode(sud2grid s)))

isMinimal :: Sudoku -> Bool
isMinimal s = (hasSingleSolution s) && (not $ any hasSingleSolution [eraseS s (i,j) |
                                                                    i <- [1..9], j <- [1..9],
                                                                    s (i,j) > 0 ])

canBeRemoved :: Sudoku -> [(Row, Column)]
canBeRemoved s = [(i,j)| i<-[1..9], j<-[1..9], s(i, j) > 0, hasSingleSolution (eraseS s (i,j))]

testMin :: Int -> Int -> IO()
testMin k n = do
        if k == n then print (show n ++ " tests passed")
          else do
            nd <- genRandomSudoku
            p <- genProblem nd
            let s = fst p
            if isMinimal s then testMin (k+1) n 
              else do
                print("Test failed, a node has multiple solutions")
                showNode nd
                error("")
runTest = do
      testMin 0 10

{-
*Lab5_3> runTest
"10 tests passed"
(110.24 secs, 34,210,325,224 bytes)

Small number of tests, because they are time-consuming
Though it looks like the genProblem does indeed generate minimal sudoku problems
-}