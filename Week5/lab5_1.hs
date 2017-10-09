-- time: 90 minutes 
module Lab5_1

where 

import Data.List
import System.Random
import Lecture5



{-
Example problem
+---------+---------+---------+
|         | 3       |         |
|   +-----|--+   +--|-----+   |
|   |     | 7|   |  | 3   |   |
| 2 |     |  |   |  |     | 8 |
+---------+---------+---------+
|   |   6 |  |   |5 |     |   |
|   +-----|--+   +--|-----+   |
|    9  1 | 6       |         |
|   +-----|--+   +--|-----+   |
| 3 |     |  | 7 |1 | 2   |   |
+---------+---------+---------+
|   |     |  |   |  |    3| 1 |
|   |8    |  | 4 |  |     |   |
|   +-----|--+   +--|-----+   |
|       2 |         |         |
+---------+---------+---------+

Example solution
+---------+---------+---------+
| 4  7  8 | 3  9  2 | 6  1  5 |
|   +-----|--+   +--|-----+   |
| 6 |1  9 | 7| 5 |8 | 3  2| 4 |
| 2 |3  5 | 4| 1 |6 | 9  7| 8 |
+---------+---------+---------+
| 7 |2  6 | 8| 3 |5 | 1  4| 9 |
|   +-----|--+   +--|-----+   |
| 8  9  1 | 6  2  4 | 7  5  3 |
|   +-----|--+   +--|-----+   |
| 3 |5  4 | 9| 7 |1 | 2  8| 6 |
+---------+---------+---------+
| 5 |6  7 | 2| 8 |9 | 4  3| 1 |
| 9 |8  3 | 1| 4 |7 | 5  6| 2 |
|   +-----|--+   +--|-----+   |
| 1  4  2 | 5  6  3 | 8  9  7 |
+---------+---------+---------+
-}

-- Grid of the provided example
exampleNrc :: Grid
exampleNrc = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]

-- Print regular rows
showRowA :: [Value] -> IO()
showRowA [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putChar ' '
     putStr (showVal a2) ; putChar ' '
     putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putChar ' '
     putStr (showVal a5) ; putChar ' '
     putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putChar ' '
     putStr (showVal a8) ; putChar ' '
     putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

-- Print row that contains part of the NRC subgrids
showRowB :: [Value] -> IO()
showRowB [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; 
     putChar ' '         ; putStr (showVal a1) ; putChar ' '
     putChar '|'         ; putStr (showVal a2) ; putChar ' '
     putChar ' '         ; putStr (showVal a3) ; putChar ' '
     putChar '|'         ; 
     putChar ' '         ; putStr (showVal a4) ; putChar '|'
     putChar ' '         ; putStr (showVal a5) ; putChar ' '
     putChar '|'         ; putStr (showVal a6) ; putChar ' '
     putChar '|'         ; 
     putChar ' '         ; putStr (showVal a7) ; putChar ' '
     putChar ' '         ; putStr (showVal a8) ; putChar '|'
     putChar ' '         ; putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

-- Show NRC grid which contains 4 extra subgrids
showGridNrc :: Grid -> IO()
showGridNrc [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+---------+---------+---------+")
    showRowA as; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRowB bs; showRowB cs
    putStrLn ("+---------+---------+---------+")
    showRowB ds; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRowA es; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRowB fs
    putStrLn ("+---------+---------+---------+")
    showRowB gs; showRowB hs; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRowA is
    putStrLn ("+---------+---------+---------+")

-- Show NRC sudoku which contains 4 extra subgrids
showSudokuNrc :: Sudoku -> IO()
showSudokuNrc = showGridNrc . sud2grid

blocksNrc :: [[Int]]
blocksNrc = [[2..4],[6..8]]

blNrc :: Int -> [Int]
blNrc x = concat $ filter (elem x) blocksNrc

subGridNrc :: Sudoku -> (Row,Column) -> [Value]
subGridNrc s (r,c) = 
  [ s (r',c') | r' <- blNrc r, c' <- blNrc c ]

freeInSubgridNrc :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridNrc s (r,c) = freeInSeq (subGridNrc s (r,c))

-- Added constraint to check for; freeInSubgridNrc
freeAtPosNrc :: Sudoku -> (Row,Column) -> [Value]
freeAtPosNrc s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c)) 
   `intersect` (freeInSubgridNrc s (r,c)) 

-- Check if NRC subgrids are injective
subgridInjectiveNrc :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNrc s (r,c) = injective vs where 
   vs = filter (/= 0) (subGridNrc s (r,c))

-- Added constraint to check for; subgridInjectiveNrc
consistentNrc :: Sudoku -> Bool
consistentNrc s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subgridInjectiveNrc s (r,c) | 
                    r <- [2,6], c <- [2,6]]

-- Replaced freeAtPos with freeAtPosNrc which checks for the extra freeInSubgridNrc constraint
constraintsNrc :: Sudoku -> [Constraint] 
constraintsNrc s = sortBy length3rd 
    [(r,c, freeAtPosNrc s (r,c)) | 
                       (r,c) <- openPositions s ]

showNodeNrc :: Node -> IO()
showNodeNrc = showSudokuNrc . fst

extendNodeNrc :: Node -> Constraint -> [Node]
extendNodeNrc (s,constraints1) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         pruneNrc (r,c,v) constraints1) | v <- vs ]

-- Added constraint "sameblockNrc"
pruneNrc :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
pruneNrc _ [] = []
pruneNrc (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | sameblockNrc (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | otherwise = (x,y,zs) : pruneNrc (r,c,v) rest

-- Check if 2 values are in the same NRC block
sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = blNrc r == blNrc x && blNrc c == blNrc y 

initNodeNrc :: Grid -> [Node]
initNodeNrc gr = let s = grid2sud gr in 
              if (not . consistentNrc) s then [] 
              else [(s, constraintsNrc s)]
              
solveNsNrc :: [Node] -> [Node]
solveNsNrc = search succNodeNrc solved 

succNodeNrc :: Node -> [Node]
succNodeNrc (s,[]) = []
succNodeNrc (s,p:ps) = extendNodeNrc (s,ps) p 

solveShowNsNrc :: [Node] -> IO[()]
solveShowNsNrc = sequence . fmap showNodeNrc . solveNsNrc

solveAndShowNrc :: Grid -> IO[()]
solveAndShowNrc gr = solveShowNsNrc (initNodeNrc gr)
