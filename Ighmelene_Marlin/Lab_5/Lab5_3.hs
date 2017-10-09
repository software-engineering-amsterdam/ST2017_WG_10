module Lab5_3

where 

import Data.List
import System.Random
import Lecture5

-- 30 min
isMinimal :: Node -> Bool
isMinimal prob = do
                  let unique  = uniqueSol prob
                  let hints   = all (\p -> (not.uniqueSol) (eraseN prob p)) (filledPositions (fst prob))
                  unique && hints

main53 :: IO ()
main53 = do 
          [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s
          putStr "isMinimal: "
          putStrLn (show(isMinimal s))
          