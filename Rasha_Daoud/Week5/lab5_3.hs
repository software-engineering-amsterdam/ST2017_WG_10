--time: 30 minutes
module Lab5_3
where

import Data.List
import System.Random
import Lab5_2

{- let's define the minimal property for a sudoku problem as follows -}
possibilitiesSudokuEraseOne :: Sudoku -> [Sudoku]
possibilitiesSudokuEraseOne sod = map (\x -> extend sod(x, 0)) $ filledPositions sod

isProblemMinimal :: Node -> Bool
isProblemMinimal node = (all (\x -> not $ uniqueSol (x, constraints x)) $ possibilitiesSudokuEraseOne (fst node))
                         &&
                        (uniqueSol node)

{- let's generate a random sudoku problem -}
genRandomSudokus :: Int -> IO [Node]
genRandomSudokus 0 = return []
genRandomSudokus n = do
        x <- genRandomSudoku
        xs <- genRandomSudokus (n-1)
        return (x:xs)

{-----------------------------------Testing using our own generator-----------------------------------}

-- Let's test any property Node -> Bool on 100 generated sudoku problem using the generator
testIter :: Int -> (Node -> Bool) -> [Node] -> IO()
testIter n p [] = print (show n ++ " tests passed...")
testIter n p (n1:ns) = if p n1 then
                         do
                          testIter n p ns
                        else
                          error ("test failed")

testSol :: Int -> (Node -> Bool) -> IO()
testSol n p = do   sols <- genRandomSudokus n
                   testIter n p sols

-- let's test any of the defined properties for 100 times
test100 :: (Node -> Bool) -> IO()
test100 p = testSol 100 p
