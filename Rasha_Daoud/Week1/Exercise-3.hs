-- Exercise 3 - time: 10 minutes
module Exercise3 where
import Data.List
import Test.QuickCheck    

-- Consider the function that produces permutations of a list of type a (given in the assignment)
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)


-- Calculate the length of perms of a list of integers from 1 to n
lenPerms :: Int -> Int
lenPerms n = length (perms [1..n])

-- Calculate the factorial of an integer n (product elements in list [1..n]
fact :: Int -> Int 
fact n = product [1..n] -- factorian n = n!

-- Check that the resulting values from lenPerms & fact on a given value n are equal
match :: Int -> Bool
match n | n> 0 = (lenPerms n == fact n)
        | otherwise = True --to not fail the check


{-
    - Give your thoughts on the following issue: when you perform the test for exercise 5, what are you testing actually?
	
	   A: quickCheck can test with any n (Int), [1..n] can be pretty huge list (based on the random selection of the QuickCheck)
		The computation power might not manage the calculation in both 'length' & 'product'.

	- When you perform the test for exercise 5, what are you testing actually? Are you checking a mathematical fact?
	   Or are you testing whether perms satisfies a part of its specification? Or are you testing something else still?
	
	   A: We're testing whether the implementaion of perms satisfies part of its specificiations
-}