-- Exercise 1 - time 15 minutes (I had to read again about Test.QuickCheck)
module Exercise1 where
import Data.List
import Test.QuickCheck


-- ex.2

-- Compute the value of applying the formula from the left side of the case
calcValL1 :: Int -> Int
calcValL1 0 = 0
calcValL1 n = sum [x^2 | x <- [1..n]]

-- Compute the value of applying the formula from the right side of the case
calcValR1 :: Int -> Int
calcValR1 0 = 0
calcValR1 n = div (n * (n+1)*(2*(n) + 1) ) 6

{- Check whether the values from the two formulas are equal
     with a check, whether the natural number is below 0, then I return the neutral Bool value True -}
match1 :: Int -> Bool
match1 n | n <0 = True
         | otherwise = (calcValL1 n == calcValR1 n)



-- ex.3

-- Compute the value of applying the formula from the left side of the case
calcValL2 :: Int -> Int
calcValL2 0 = 0
calcValL2 n = sum [x^3 | x <- [1..n]]

-- Compute the value of applying the formula from the right side of the case
calcValR2 :: Int -> Int
calcValR2 0 = 0
calcValR2 n = (div (n*(n+1)) 2)^2


{- Check whether the values from the two formulas are equal
    with a check, whether the natural number is below 0, then I return the neutral Bool value True -}
match2 :: Int -> Bool
match2 n | n <0 = True 
         | otherwise = (calcValL2 n == calcValR2 n)


 
{- To test whether both assumptions are True in GHCi type the following 
	command: quickCheck match1
	result: +++ OK, passed 100 tests.

	command: quickCheck match2
	result: +++ OK, passed 100 tests.

    **Conclusion: I've proven our assumptions are true, by applying quick check (100 random ns)
-}
