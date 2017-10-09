module Lab5_4

where 

import Data.List
import System.Random
import Lecture5

-- 30 min
minimalizeN :: Node -> [(Row,Column)] -> Int -> Node
minimalizeN n [] blanks = n
minimalizeN n ((r,c):rcs) blanks  | length (openPositions (fst n)) == blanks = n
                                  | uniqueSol n' = minimalizeN n' rcs blanks
                                  | otherwise    = minimalizeN n  rcs blanks
  where n' = eraseN n (r,c)

genProblemN :: Node -> Int -> IO Node
genProblemN n blanks = do 
                        ys <- randomize xs
                        return (minimalizeN n ys blanks)
                        where xs = filledPositions (fst n)

main54_3 :: IO ()
main54_3 = main54_n 3

main54_n :: Int -> IO ()
main54_n n = do 
              [r] <- rsolveNs [emptyN]
              showNode r
              s  <- genProblemN r n
              showNode s
