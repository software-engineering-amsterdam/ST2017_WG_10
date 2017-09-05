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
-- add up digits of doubled numbers that are > 10 -----break them into their digits and sum the digits
doubleNext :: [Integer] -> [Integer]
doubleNext [] = []
doubleNext [x] = [x]
doubleNext (n:m:xs) = if (2*m <10)
                        then (n: (2*m): doubleNext xs )
                      else (n: (sum(int2ListInt (2*m)): doubleNext xs )) -- sum digits of the the doubled every other 

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

{- To verify if our implementation is correct,
   it would be good to test whether we pass a valid credit-card number, and the result of our validator is as expected.
	and if we pass invalid card-number, the function should return false
 -}

-- Consider the list of pairs, first element is the card-number and the second element is the validator expected result
americanEValidity :: [(Integer, Bool)]
americanEValidity = [
                             (342238480772410, True),
                             (343186419945624, True),
                             (379773487441912, True),
                             (5246644310785941, False)
                           ]

{- numbers in the previous list are taken from https://www.freeformatter.com/credit-card-number-generator-validator.html -}


-- we can test any of our validators against a defined list we fill it with a number and the expected result of the validation
testValidity :: (Integer -> Bool) -> [(Integer, Bool)] -> Bool
testValidity f l = and [f n == v  | (n,v) <- l]
-- the function f can be isMaster, isVisa, isAmericanExpress or simple luhn
-- the list l can be americanEValidity or any list of card numbers (integers)

{- GHCi:
         *Exercise7> testValidity isAmericanExpress americanEValidity
          True
-}

{-
  basically we can define any list rather than americanEValidity and call testValidity on the relevant validator and the list
  (Visa card & Master card)
-}


{- we can also implement a generator using System.Random to produce seeds, to fill the list which we will apply testValidity on.
   e.g implement a generator of american express card numbers (following our specifications).
   but that might not make sense, because the generator will follow same specifications as the validator of the relevant type of cards.
   therefore I find prove by example more sufficient.
-}
