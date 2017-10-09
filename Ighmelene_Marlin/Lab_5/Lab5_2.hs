module Lab5_2

where 

import Data.List
import System.Random
import Lecture5
import Lab5_1

-- 180 min
{-
This second version is easier to extend for NRC problems because I just create an extra constraint and add it to the constraint collection for solving a basic sudoku problem.
I join the rowConstrnt, columnConstrnt and blockConstrnt in one constraint: sudoku.

To modify for NRC sudoku I had to add the following methods:
  blocksNrc,
  nrcConstrnt.
And modify the following methods:
  sudoku,
  showNode (to print NRC sudoku showing extra subgrids).

But in the first version I had to add the following methods:
  blocksNrc,
  blNrc,
  subGridNrc,
  freeInSubgridNrc,
  subgridInjectiveNrc,
  sameblockNrc.
And modify the following methods:
  freeAtPos,
  consistent,
  prune.
-}

type Position = (Row,Column)
type Constrnt = [[Position]]

rowConstrnt,columnConstrnt,blockConstrnt,nrcConstrnt :: Constrnt
rowConstrnt     = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt  = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt   = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks,     b2 <- blocks ]
nrcConstrnt     = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocksNrc,  b2 <- blocksNrc]

-- Indicate what type of sudoku you want to solve
sudoku, original, nrc :: Constrnt
original  = union (union rowConstrnt columnConstrnt) blockConstrnt
nrc       = union original nrcConstrnt
sudoku    = nrc

-- Indicate what type of sudoku you want to show
showMethod, showOriginal, showNrc :: Sudoku -> IO ()
showOriginal  = showSudoku
showNrc       = showSudokuNrc
showMethod    = showNrc

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = 
                        let ys = filter (elem (r,c)) xs 
                          in 
                        foldl1 intersect (map ((values \\) . map s) ys)

consistent' :: Sudoku -> Bool
consistent' s = all injective (map (\c -> filter (/=0) (map (\p -> s p) c)) sudoku)

constraints' :: Sudoku -> [Constraint] 
constraints' s = sortBy length3rd 
    [(r,c, freeAtPos' s (r,c) sudoku) | 
                       (r,c) <- openPositions s ]

initNode' :: Grid -> [Node]
initNode' gr = let s = grid2sud gr in 
              if (not . consistent') s then [] 
              else [(s, constraints' s)]

solveNs' :: [Node] -> [Node]
solveNs' = search succNode' solved 

succNode' :: Node -> [Node]
succNode' (s,[]) = []
succNode' (s,p:ps) = extendNode' (s,ps) p 

extendNode' :: Node -> Constraint -> [Node]
extendNode' (s,constraints') (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune' (r,c,v) constraints') | v <- vs ]

prune' :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune' _ [] = []
prune' (r,c,v) ((x,y,zs):rest)
  | shareConstrnt (r,c) (x,y) sudoku = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | otherwise = (x,y,zs) : prune' (r,c,v) rest

shareConstrnt :: Position -> Position -> Constrnt -> Bool
shareConstrnt p1 p2 xs = any (\c -> elem p1 c && elem p2 c) xs

showNode' :: Node -> IO()
showNode' = showMethod . fst

solveShowNs' :: [Node] -> IO[()]
solveShowNs' = sequence . fmap showNode' . solveNs'

solveAndShow' :: Grid -> IO[()]
solveAndShow' gr = solveShowNs' (initNode' gr)
