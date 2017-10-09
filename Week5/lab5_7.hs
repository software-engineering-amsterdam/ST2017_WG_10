--time: 20 minutes
module Lab5_7
where

import Data.List
import System.Random
import Lecture5

{- Minimal problems for NRC Sudokus need fewer hints than standard Sudoku problems. Investigate the difference. What is the average number of hints in a minimal standard Sudoku problem? What is the average number of hints in a minimal NRC Sudoku problem? -}


{- let's generate n number of problems and get the average -}
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

To test NRC sudoku, we can change the import Lecture5.hs to use Lab5_1.hs

in this case, the problem is adjusted to be NRC:

the result of the application on 20 NRC problems is as follows:

   *Lab5_7> averageFilledPos 0 20 0
    25.45

We can calculate the average on more than 20 problems. That took longer, but the results showed that the average
of a normal sudoku problem is always lower.
-}


{- remove the comment for the following code in case you want to use Lab5_1.hs 
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
-}