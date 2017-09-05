-- Exercise 7 -time: 2 hours
module Exercise7 where
import Data.List
import Test.QuickCheck 
import System.Random

-- first we need a conversion function from integer to a list of all digits of that integer 
int2ListInt :: Integer -> [Integer]
int2ListInt 0 = []
int2ListInt n | (n<10) = [n] 
              | otherwise = int2ListInt (div n 10) ++ [mod n 10] 

-- second, we need a function to double the next digit in the list (weights of product are: 1 2 1 2 1 2 ... etc ..)
doubleNext :: [Integer] -> [Integer]
doubleNext [] = []
doubleNext [x] = [x]
doubleNext (n:m:xs) = n: (2*m): doubleNext xs

-- Luhn function, verifies if an integer satisfies the algorithm
luhn :: Integer -> Bool
luhn n =  mod (sum (doubleNext (int2ListInt n))) 10 == 0
-- GHCi:  luhn 24 = True - > 2*1 + 4 * 2 = 10  correct


{-------------------------------------------------------------------------------------------------------------------------}

-- Verify if a number starts with another number
isStartingBy :: Integer -> Integer -> Bool
isStartingBy n m = or [xx == yy | (xx,yy)<- zip (int2ListInt n) (int2ListInt m)]

-- Second step, use previous function to check whether a number starts with a list of other numbers (options)
isStartingByOptions :: Integer -> [Integer] -> Bool
isStartingByOptions n ms = or [isStartingBy n m | m <-ms]

{- GHCi:
	*Exercise7> isStartingByOptions 37545343443 [34, 37]
	True
-}


{- Giving the properties of the different types of cards (properties were on internet)
   we can now implement the three validators for the three types of cards mentioned in the assignment: -}
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = 
                      elem (length (int2ListInt n)) [15] --length (int2ListInt n) == 15
                      &&
                      luhn n
                      && isStartingByOptions n [34, 37]

isMaster :: Integer -> Bool
isMaster n =          elem (length (int2ListInt n)) [16]
                      &&
                      luhn n
                      && isStartingByOptions n ([51, 52, 53, 54, 55] ++ [222100..272099])

isVisa :: Integer -> Bool
isVisa n =            elem (length (int2ListInt n)) [13,16,19]
                      &&
                      luhn n
                      && isStartingByOptions n [4]


{-------------------------------------------------------------------------------------------------------------------------}

{- To test the implemenation, we need to design a kind of random generator for numbers
    first, it would be good to test whether we pass a valid credit-card number, and the result is validation is as expected.
	and if we pass invalid card-number, the function should return false
 -}
{- still TODO --}

-- then we implement a simple generator
generatorNrs :: Int -> Int -> [Integer]
generatorNrs 0 _ = []
generatorNrs n seed = take n . randomRs (0, 9) . mkStdGen $ seed -- n is the length we need
		  
-- Get the sum of all digits returned from the generator (to test against Luhn or other customized validators)
getSumOnGeneratedNr :: [Integer] -> Integer
getSumOnGeneratedNr ns = (sum.doubleNext) ns 

