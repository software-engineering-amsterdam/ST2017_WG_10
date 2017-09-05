module Lab1_1 where
import Data.List
import Test.QuickCheck  

-- works only for natural n and zero. Haskell has no natural number type, so I improvised that for negative n the result is true
-- so as to not interfere with testing of other integers
squares :: Integer -> Bool
squares n
    | n < 0     = True
    | otherwise = sum (map (\x -> x*x) [1 .. n]) == n*(n+1)*(2*n+1)`div`6

cubes :: Integer -> Bool
cubes n
    | n < 0     = True
    | otherwise = sum (map (\x -> x*x*x) [1 .. n]) == (n*(n+1)`div`2)^2

--2 min