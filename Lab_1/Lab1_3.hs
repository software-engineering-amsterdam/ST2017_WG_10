module Lab1 where
import Data.List
import Test.QuickCheck

-- 45 min
perms :: [Int] ->[[Int]]
perms []      = [[]]
perms (x:xs)  = concat (map (insrt x) (perms xs)) where 
                insrt x [] = [[x]]
                insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)
                
factorial :: [Int] -> Int
factorial [] = 1
factorial xs = length xs * factorial (tail xs)

p :: [Int] -> Bool
p xs  = length (permutations (take maxL xs)) == factorial (take maxL xs) where maxL = 9

-- No, it was not hard to test.
-- It tests both a mathematical fact; that the number of possible permutations is n!
