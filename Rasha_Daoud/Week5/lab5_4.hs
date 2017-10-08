--time: 1 hour
module Lab5_4
where

import Data.List
import System.Random
import Lab5_2
import Lab5_3

{- Write a program that generates Sudoku problems with three empty blocks. Is it also possible to generate Sudoku problems with four empty blocks? Five? How can you check this? -}

-- a function to erase one block out of 9 (by block number)
blockErase :: Integer -> Node -> Node
blockErase id node = erase b node
                     where b = [(x, y) | x <-  (blocks !! (fromIntegral (div id 3))),
                                y <- (blocks !! (fromIntegral (mod id 3)))]

-- a function that erases many blocks
blocksErase :: [Integer] -> Node -> Node
blocksErase [] node       = node
blocksErase (id:ids) node = blocksErase ids (blockErase id node)

-- a function to erase a list of cells (Row, Column) based on eraseN from Lecture5.hs
erase :: [(Row,Column)] -> Node -> Node
erase [] node         = node
erase ((r,c):ps) node = erase ps (eraseN node (r,c))



-- generate random sudoku with n blocks erased
generateProblemNEmptyBlock :: Int -> IO Node
generateProblemNEmptyBlock n = do 
        sod <- genRandomSudoku
        l <- randomize [0..8]
        let p = blocksErase (take n l) sod in
          if uniqueSol p || n>4 then return p
		  else generateProblemNEmptyBlock n

-- main generator for sudoku problem with n blocks erased
getSudoku :: Int -> IO Node
getSudoku n = do 
                 p <- generateProblemNEmptyBlock n
                 genProblem p

execute = do
         print "sudoku problem with 3 erased blocks:";
		 minProb <- getSudoku 3;
         showNode minProb;
		 print "sudoku problem with 4 erased blocks:";
		 minProb <- getSudoku 4;
         showNode minProb;
		 print "sudoku problem with 5 erased blocks:";
		 prob <- getSudoku 5;
		 showNode prob;
		 print "trying to solve prob with 5 erased blocks....";
		 solveAndShow (sud2grid (fst prob));
{-

*Lab5_4> execute
"sudoku problem with 3 erased blocks:"
+-------+-------+-------+
|     9 | 4   7 |       |
|       | 6 3 9 |       |
|   5   |       |       |
+-------+-------+-------+
|     1 |       |       |
| 8     |   7 3 |       |
| 4 6 2 | 9     |       |
+-------+-------+-------+
| 2     |       |   8 4 |
|       |       | 2 6 3 |
|     4 |       | 5   7 |
+-------+-------+-------+
"sudoku problem with 4 erased blocks:"
+-------+-------+-------+
|       |   6   | 1     |
|       |       | 9 2   |
|       | 4 2 5 |     7 |
+-------+-------+-------+
|       | 3 8   | 4 9   |
|       |   1   |   3 6 |
|       |   5 2 |   8   |
+-------+-------+-------+
| 2 9 8 |       |       |
| 3 4 6 |       |       |
| 7 5   |       |       |
+-------+-------+-------+
"sudoku problem with 5 erased blocks:"
+-------+-------+-------+
| 7 5 6 |       | 3 9 4 |
| 1 8 3 |       | 2 7 6 |
| 2 9 4 |       | 5 8 1 |
+-------+-------+-------+
|       |       | 7 4 8 |
|       |       | 9 5 3 |
|       |       | 1 6 2 |
+-------+-------+-------+
|       | 3 7 8 |       |
|       | 4 1 5 |       |
|       | 2 6 9 |       |
+-------+-------+-------+

If we try to generate sudoku with 5 erased blocks, we always get so far the other 4 blocks completely filled.

If we try to solve the sudoku problem with 5 erased blocks, there are more than one possible solution. 
but the sudoku problem is not minimal, if more than 4 blocks are empty.
-}
