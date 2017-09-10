-- Exercise 6 -time: 30 minutes
module Exercise6 where
import Data.List
import Test.QuickCheck 

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
           where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..] 

-- Calulcate the product of the first n prime numbers
prodCalc :: Int -> Integer
prodCalc n = product (take n primes) + 1

-- Using a list of integer as var, filter all non prime numbers in the prodCalc of [2..].
-- The list should not be empty which means we refute the fact.
refuteFact :: [Integer]
refuteFact = filter (not.prime) (map prodCalc [2..])

{- To test it in GHCi, type refuteFact
    it will return non empty list. Then we've proven that the fact is false.
	
    result is [30031,510511,9699691,223092871,6469693231 ... ..... .....]
	
	-What is the smallest counterExample?
	GHCi:
			head refuteFact
			30031
	
-}

{- We've chosen this solution because it is short, and easy to read -}