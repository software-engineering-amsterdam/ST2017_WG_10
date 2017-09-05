module Lab1_3 where
import Data.List
import Test.QuickCheck  

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

permsCorrect :: Int -> Bool
permsCorrect n
    | n < 1 = True
    | otherwise = length(perms n) == foldl (*) 1 [1 .. n]
    
{-
Same as with power set, this test takes much time because the permutation number grows exponentially.
And also the same, we are testing number of lists that `perms` outputs, which might not be the same as number of
permutations if `perms` is not implemented correctly.
-}
--1 min