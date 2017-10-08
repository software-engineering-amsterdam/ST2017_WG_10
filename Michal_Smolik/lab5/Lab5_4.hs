module Lab5_4
where

import Data.List
import System.Random
import Lecture5

eraseBlock :: (Int, Int) -> Node -> Node
eraseBlock (r, c) nd = eraseList [(x, y)| x <- [((r+1)*3 - 2) .. ((r+1)*3)], y <- [((c+1)*3-2) .. ((c+1)*3)]] nd

eraseList :: [(Row, Column)] -> Node -> Node
eraseList [] nd = nd
eraseList (x:xs) nd = eraseList xs (eraseN nd x)

nToCoords :: Int -> (Int, Int)
nToCoords n = (n `div` 3, n `mod` 3)

eraseMultipleBlocks :: [Int] -> Node -> Node
eraseMultipleBlocks [] nd = nd
eraseMultipleBlocks (x:xs) nd = eraseMultipleBlocks xs $ eraseBlock (nToCoords x) nd

genMissingN :: Int -> IO(Node)
genMissingN n = do
        s <- genRandomSudoku
        xs <- randomize [0..8]
        let nd = eraseMultipleBlocks (take n xs) s
        if hasSingleSolution nd || n > 4 then (return nd)
            else do  genMissingN n


hasSingleSolution :: Node -> Bool
hasSingleSolution nd = 1 == length(solveNs([nd]))

genProbMissingN :: Int -> IO(Node)
genProbMissingN n = do
            nd <- genMissingN n
            p <- genProblem nd
            return p

run = do
    missingthree <- genProbMissingN 3
    showNode missingthree
    missingfour <- genProbMissingN 4
    showNode missingfour
    missingfive <- genProbMissingN 5
    showNode missingfive

{-
Apparently we can make a sudoku problem with 3 empty blocks and a single solution pretty easily.
For four empty ones we have to wait a little, and for five I didn't manage to find a problem
with a single solution. 
-}