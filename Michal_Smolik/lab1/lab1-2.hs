module Lab1_2 where
import Data.List
import Test.QuickCheck  

powerSet :: [a] -> Bool
powerSet x = length(subsequences x) == 2 ^ length(x)

{-
This thing takes too long, because the size of `subsequences x` grows exponentially, so if quickCheck tries a large
number like 100, the computation will take unreasonable amount of time.

This is just a test of implementation of subsequences, since we aren't testing the power set size,
but size of output of `subsequences x`. This is also not even a complete
test, as it doesn't chceck for correctness of the generated subsets, just their count.
-}
--1 min