-- Prime permutations  -time: 30 mins
import Data.List
import Test.QuickCheck

-- The following two functions are given in previous assignments
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
           where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

{-  https://projecteuler.net/problem=49		
	The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
	There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
	What 12-digit number do you form by concatenating the three terms in this sequence?
-}

-- we already know the step between two sequencial numbers
increaseStep = 3330

-- first we need a conversion function from integer to a list of all digits of that integer (exercise 7)
int2ListInt :: Integer -> [Integer]
int2ListInt 0 = []
int2ListInt n | (n<10) = [n] 
              | otherwise = int2ListInt (div n 10) ++ [mod n 10]

-- secondly, we need to implement a function to decide whether two numbers are permutations of one another
isPermTwoNrs :: Integer -> Integer -> Bool
isPermTwoNrs a b = and [elem x (int2ListInt b) | x<-(int2ListInt a)]
                                

-- find the three terms that follow : [a, a+increaseStep, a+increaseStep*2] & isPermTwoNrs each sequencial numbers in the list
findTerms :: [Integer]
findTerms = head (tail[[a, a+increaseStep, a+increaseStep*2] | a <-  [1..], (prime a) && a <8147 && a >1487
                        && (isPermTwoNrs a (a+increaseStep)) && (isPermTwoNrs (a+increaseStep) (a+increaseStep*2)) ])
	 			
-- 12 digits into [Integer], each element is a digit
solution :: [Integer] -> [Integer]
solution [] = []
solution (x:xs) = (int2ListInt x) ++ solution xs
		
{- GHCi: 
    *Main> findTerms
	[2591,5921,9251]
	
	*Main> solution findTerms
    [2,5,9,1,5,9,2,1,9,2,5,1]
	
-}


{- We've chosen this solution, because only one member solved problem49 -}