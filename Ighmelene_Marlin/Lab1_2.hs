module Lab1 where
import Data.List
import Test.QuickCheck

-- 30 min
f :: [Int] -> Int
f n = length (subsequences n)
g :: [Int] -> Int
g n = 2 ^ (length n)
p :: [Int] -> Bool
p n = f (take maxN n) == g (take maxN n) where maxN = 20

-- No, it was not hard to test.
-- It tests both a mathematical fact; that the number of possible subsequences is 2^n
