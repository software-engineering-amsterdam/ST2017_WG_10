module Lab1 where
import Data.List
import Test.QuickCheck

-- 25 min
f1 :: Int -> Int
f1 n = sum [ x * x | x <- [1..n] ]
g1 :: Int -> Int
g1 n = div ( n * (n+1) * ( (2 * n) + 1) ) 6
p1 :: Int -> Bool
p1 n = f1 (abs n) == g1 (abs n)

-- 5 min
f2 :: Int -> Int
f2 n = sum [x^3 | x <- [1..n]]
g2 :: Int -> Int
g2 n = ( div (n * (n+1)) 2 ) ^ 2
p2 :: Int -> Bool
p2 n = f2 (abs n) == g2 (abs n)
