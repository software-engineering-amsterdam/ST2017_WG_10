-- Exercise 2 - time: 10 minutes
module Exercise2 where
import Data.List
import Test.QuickCheck    


{- we're trying to prove that:
  if length [a] = n   then  length subsequences [a] = 2^n
-}

-- Compute the length of the subsequences of a list from 1 to n
calcValL1 :: Int -> Int
calcValL1 n = length (subsequences [1..n])


-- Compute the result of applying the formula on the right side
calcValR1 :: Int -> Int
calcValR1 0 = 0
calcValR1 n = 2^(length [1..n])


{- Compare the result from previous functions for the same n
    check also whether n in the list is a positive integer >0 (according to the question) -}
match :: Int -> Bool
match n  | n > 0 = calcValL1 n == calcValR1 n
          | otherwise = True --to not fail the check


{- Is the property hard to test? If you find that it is, can you given a reason why?
	
	A: Yes, it is hard to test
	   n provided by the quickCheck can be big number, therefore the list [1..n] can be already huge to obtain length on.
		 But the real problem is that subsequences can grow pretty exponential (length 2^n). The list subsequences can be huge.
		 If the length of the original list is 10000, the subsequences list will be 2^10000 
		 The computation power might not manage executing length on such huge lists.
-}

{-- Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually? 
		Are you checking a mathematical fact? 
		Or are you testing whether subsequences satisfies a part of its specification?
		Or are you testing something else still?

	A: I'm trying to verify that the implementation of the function 'subsequences' satisfies the mathimatical statement (fact) in exercise4
--}