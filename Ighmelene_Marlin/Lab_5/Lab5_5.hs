module Lab5_5

where 

import Data.List
import System.Random
import Lecture5
import Lab5_2

-- 60 min
{-
*Lab5_5> main55
+---------+---------+---------+
| 8  3  9 | 6  7  2 | 4  1  5 |
|   +-----|--+   +--|-----+   |
| 5 |1  7 | 8| 9 |4 | 2  3| 6 |
| 4 |6  2 | 5| 3 |1 | 7  9| 8 |
+---------+---------+---------+
| 1 |9  3 | 4| 2 |8 | 6  5| 7 |
|   +-----|--+   +--|-----+   |
| 6  2  4 | 7  1  5 | 3  8  9 |
|   +-----|--+   +--|-----+   |
| 7 |8  5 | 3| 6 |9 | 1  2| 4 |
+---------+---------+---------+
| 2 |4  1 | 9| 8 |7 | 5  6| 3 |
| 9 |7  6 | 2| 5 |3 | 8  4| 1 |
|   +-----|--+   +--|-----+   |
| 3  5  8 | 1  4  6 | 9  7  2 |
+---------+---------+---------+

+---------+---------+---------+
|    3    |         |         |
|   +-----|--+   +--|-----+   |
| 5 |   7 |  |   |  | 2   |   |
|   |6    |  | 3 |  | 7   |   |
+---------+---------+---------+
|   |     | 4|   |  |     |   |
|   +-----|--+   +--|-----+   |
|         | 7       |    8    |
|   +-----|--+   +--|-----+   |
|   |     |  |   |  |     |   |
+---------+---------+---------+
|   |     |  | 8 |  | 5   |   |
| 9 |     |  |   |  |     | 1 |
|   +-----|--+   +--|-----+   |
|       8 |       6 |       2 |
+---------+---------+---------+

-------------------------------

*Lab5_5> main55
+---------+---------+---------+
| 9  2  4 | 6  7  5 | 3  1  8 |
|   +-----|--+   +--|-----+   |
| 8 |5  1 | 2| 9 |3 | 7  6| 4 |
| 6 |3  7 | 8| 1 |4 | 9  5| 2 |
+---------+---------+---------+
| 7 |6  9 | 4| 5 |2 | 1  8| 3 |
|   +-----|--+   +--|-----+   |
| 5  1  8 | 9  3  7 | 4  2  6 |
|   +-----|--+   +--|-----+   |
| 3 |4  2 | 1| 8 |6 | 5  7| 9 |
+---------+---------+---------+
| 4 |9  6 | 7| 2 |1 | 8  3| 5 |
| 1 |8  5 | 3| 6 |9 | 2  4| 7 |
|   +-----|--+   +--|-----+   |
| 2  7  3 | 5  4  8 | 6  9  1 |
+---------+---------+---------+

+---------+---------+---------+
|       4 |    7    | 3       |
|   +-----|--+   +--|-----+   |
|   |     |  |   |  |    6| 4 |
|   |     | 8| 1 |  |     |   |
+---------+---------+---------+
|   |     |  |   |  |    8|   |
|   +-----|--+   +--|-----+   |
|       8 |         | 4  2    |
|   +-----|--+   +--|-----+   |
|   |     | 1|   |6 |     |   |
+---------+---------+---------+
|   |   6 | 7| 2 |  |     |   |
|   |     |  |   |  |     |   |
|   +-----|--+   +--|-----+   |
|    7    | 5       |         |
+---------+---------+---------+
-}

main55 :: IO ()
main55 = do 
          [r] <- rsolveNs' [emptyN]
          showNode' r
          putStrLn ""
          s  <- genProblem' r
          showNode' s

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

