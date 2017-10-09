--time: 180 minutes 
module Lab5_2

where 

import Data.List
import System.Random
import Lecture5
import Control.Exception
import Data.Time


{-
This second version is easier to extend for NRC problems, because we just create an extra constraint and add it to the constraint collection for solving a basic sudoku problem.
We join the rowConstrnt, columnConstrnt and blockConstrnt in one constraint: sudoku.

To modify for NRC sudoku we had to add the following methods:
  blocksNrc,
  nrcConstrnt.
And modify the following methods:
  sudoku,
  showNode (to print NRC sudoku showing extra subgrids).

But in the first version we had to add the following methods:
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

Extandability: The new Constrnt type makes extending easier
Efficiency: The refactored code takes longer to generate and solve a problem

Original code
"Problem:"
+-------+-------+-------+
| 7   3 | 8     |       |
|   6   |       |       |
|       |   6   |   1 8 |
+-------+-------+-------+
|       |   7 3 |     9 |
|       | 2   6 |   4 3 |
|     7 |       | 8   6 |
+-------+-------+-------+
|   2 1 |       | 3     |
|     9 |       |   8   |
|       | 5 2 1 |       |
+-------+-------+-------+
"Solution:"
+-------+-------+-------+
| 7 1 3 | 8 4 5 | 6 9 2 |
| 8 6 4 | 9 1 2 | 5 3 7 |
| 9 5 2 | 3 6 7 | 4 1 8 |
+-------+-------+-------+
| 2 8 6 | 4 7 3 | 1 5 9 |
| 1 9 5 | 2 8 6 | 7 4 3 |
| 3 4 7 | 1 5 9 | 8 2 6 |
+-------+-------+-------+
| 4 2 1 | 7 9 8 | 3 6 5 |
| 5 7 9 | 6 3 4 | 2 8 1 |
| 6 3 8 | 5 2 1 | 9 7 4 |
+-------+-------+-------+



"done testing the solver on random sudoku problems..."
2.75181s

Refactored code
"Problem:"
+-------+-------+-------+
| 2     |   4   | 7 1   |
|       |       |     4 |
|       |   8 7 |   9 3 |
+-------+-------+-------+
|   5 4 |     1 |       |
|       |       | 9 8   |
|     1 |       | 6     |
+-------+-------+-------+
| 1   7 | 2     |       |
| 8 4 3 |   6   |       |
|       |     9 |       |
+-------+-------+-------+
"Solution:"
+-------+-------+-------+
| 2 3 9 | 5 4 6 | 7 1 8 |
| 7 1 8 | 9 2 3 | 5 6 4 |
| 4 6 5 | 1 8 7 | 2 9 3 |
+-------+-------+-------+
| 6 5 4 | 8 9 1 | 3 7 2 |
| 3 7 2 | 6 5 4 | 9 8 1 |
| 9 8 1 | 3 7 2 | 6 4 5 |
+-------+-------+-------+
| 1 9 7 | 2 3 8 | 4 5 6 |
| 8 4 3 | 7 6 5 | 1 2 9 |
| 5 2 6 | 4 1 9 | 8 3 7 |
+-------+-------+-------+



"done testing the solver on random sudoku problems..."
4.384486s
-}

-- Timer in main52 taken from the following site to test how long it takes to generate and solve a sudoku problem
-- http://chrisdone.com/posts/measuring-duration-in-haskell

main52 :: IO ()
main52 = do
            putStrLn "Original code"
            start <- getCurrentTime
            randomTester 1
            end <- getCurrentTime
            print (diffUTCTime end start)
            putStrLn "\nRefactored code"
            start <- getCurrentTime
            randomTester1 1
            end <- getCurrentTime
            print (diffUTCTime end start)


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


-------
rsuccNode' :: Node -> IO [Node]
rsuccNode' (s,cs) = do 
                      xs <- getRandomCnstr cs
                      if null xs 
                        then return []
                        else return (extendNode' (s,cs\\xs) (head xs))

rsolveNs' :: [Node] -> IO [Node]
rsolveNs' ns = rsearch rsuccNode' solved (return ns)

uniqueSol' :: Node -> Bool
uniqueSol' node = singleton (solveNs' [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

minimalize' :: Node -> [(Row,Column)] -> Node
minimalize' n [] = n
minimalize' n ((r,c):rcs) | uniqueSol' n' = minimalize' n' rcs
                          | otherwise     = minimalize' n  rcs
                          where n' = eraseN' n (r,c)

eraseN' :: Node -> (Row,Column) -> Node
eraseN' n (r,c) = (s, constraints' s) 
                  where s = eraseS (fst n) (r,c) 

genRandomSudoku' :: IO Node
genRandomSudoku' = do 
                    [r] <- rsolveNs' [emptyN]
                    return r

genProblem' :: Node -> IO Node
genProblem' n = do 
                  ys <- randomize xs
                  return (minimalize' n ys)
                  where xs = filledPositions (fst n)

------

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

randomTester :: Int -> IO ()       
randomTester 0 = print("done testing the solver on random sudoku problems...")
randomTester n = do     r <- genRandomSudoku
                        s <- genProblem r
                        print ("Problem:")
                        showSudoku (fst s)
                        print ("Solution:")
                        showSudoku (fst $ head $ solveNs [s])
                        putStr "\n\n\n"
                        randomTester (n-1)

randomTester1 :: Int -> IO ()       
randomTester1 0 = print("done testing the solver on random sudoku problems...")
randomTester1 n = do    
                        r <- genRandomSudoku'
                        s <- genProblem' r
                        print ("Problem:")
                        showSudoku (fst s)
                        print ("Solution:")
                        showSudoku (fst $ head $ solveNs' [s])
                        putStr "\n\n\n"
                        randomTester (n-1)
