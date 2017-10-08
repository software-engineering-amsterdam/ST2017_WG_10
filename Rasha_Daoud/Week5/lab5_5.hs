--time: 30 minutes
module Lab5_5
where

import Data.List
import System.Random
import Lab5_1 -- import to reuse functions

{- Extend the code of the lectures to create a program that generates NRC Sudoku problems, that is, Sudoku problems satisfying the extra constraint explained in the NRC exercise above. -}

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs 
                        then return []
                        else return 
                          (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node]) 
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else 
                             rsearch 
                               succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s) 
  where s = eraseS (fst n) (r,c) 

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

randomTester 0 = print("done testing the solver on random sudoku problems...")
randomTester n = do     r <- genRandomSudoku
                        s <- genProblem r
                        print ("Problem:")
                        showSudoku (fst s)
                        print ("Solution:")
                        showSudoku (fst $ head $ solveNs [s])
                        putStr "\n\n\n"
                        randomTester (n-1)

main = do
         randomTester 100

{-

"Problem:"
+-------+-------+-------+
|   4 5 |       |       |
|       | 7     |   5 1 |
|   6 9 |     5 | 7 4 3 |
+-------+-------+-------+
|   7   |       | 4     |
|   2   |       |       |
|   8   | 3   1 |     9 |
+-------+-------+-------+
|   9 3 | 8     | 1 2   |
|       |     2 |       |
|       |   5   |       |
+-------+-------+-------+

"Solution:"
+-------+-------+-------+
| 3 8 7 | 1 2 6 | 9 4 5 |
| 4 9 5 | 7 8 3 | 1 2 6 |
| 2 1 6 | 4 9 5 | 7 3 8 |
+-------+-------+-------+
| 8 5 4 | 3 7 9 | 6 1 2 |
| 7 2 1 | 5 6 4 | 8 9 3 |
| 6 3 9 | 2 1 8 | 5 7 4 |
+-------+-------+-------+
| 1 7 8 | 6 4 2 | 3 5 9 |
| 5 6 2 | 9 3 1 | 4 8 7 |
| 9 4 3 | 8 5 7 | 2 6 1 |
+-------+-------+-------+



"Problem:"
+-------+-------+-------+
|       |       |       |
|     8 | 9     |   5   |
|       | 8   5 | 9 1   |
+-------+-------+-------+
|       | 2 8   | 1     |

|       | 4 3   |     7 |
|     3 |       | 6 2   |
+-------+-------+-------+
|     5 | 3 9 2 | 7 6   |
| 2     | 5     | 4     |
| 3     |   6   |       |
+-------+-------+-------+
"Solution:"
+-------+-------+-------+
| 5 9 2 | 6 1 3 | 8 7 4 |
| 6 1 8 | 9 4 7 | 3 5 2 |
| 7 3 4 | 8 2 5 | 9 1 6 |
+-------+-------+-------+
| 9 5 7 | 2 8 6 | 1 4 3 |
| 8 2 6 | 4 3 1 | 5 9 7 |
| 1 4 3 | 7 5 9 | 6 2 8 |
+-------+-------+-------+
| 4 8 5 | 3 9 2 | 7 6 1 |
| 2 6 1 | 5 7 8 | 4 3 9 |
| 3 7 9 | 1 6 4 | 2 8 5 |
+-------+-------+-------+

.............
.............
-}