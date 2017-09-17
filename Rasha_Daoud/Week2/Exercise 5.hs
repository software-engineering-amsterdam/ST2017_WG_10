--Recognizing and generating derangements - time: 40 mins
module Exercise5 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{- implemented in exercise 4 -}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []       = True
isPermutation [] _        = False
isPermutation _ []        = False
isPermutation (p:px) qx   | elem p qx = isPermutation px (delete p qx)
                          | otherwise = False

{- Give a Haskell implementation of a property isDerangement that checks whether one list is a derangement of another one. -}

-- first, we need a function to check that there are no elements in the two lists that share the same position and they're equal
checkSelf :: Eq a => [a] -> [a] -> Bool
checkSelf [] []           = True
checkSelf [] _            = False
checkSelf _ []            = False
checkSelf (x:xs) (y:ys)   | x /= y = checkSelf xs ys -- in the same position
                          | otherwise = False

-- secondly, we can implement isDerangement to check whether the elements in the same position do not match, and the lists are permutation of one another 
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = checkSelf xs ys && isPermutation xs ys

{- GHCi:
    *Exercise5> isDerangement [1,2,3] [2,3,1]
    True
    *Exercise5> isDerangement [1,2,3] [3,2,1]
    False
-}

{- Give a Haskell implementation of a function deran that generates a list of all derangements of the list [0..n-1].-}
--permutations :: [a] -> [[a]]
deran :: Int -> [[Int]]
deran n =  [xs | xs <- permutations ns, isDerangement xs ns]
           where ns = [0..(n-1)]

{-
	*Exercise5> deran 4
	[[3,2,1,0],[2,3,1,0],[1,2,3,0],[3,0,1,2],[1,3,0,2],[1,0,3,2],[3,2,0,1],[2,3,0,1],[2,0,3,1]]
-}

{- Next, define some testable properties for the isDerangement function, and use some well-chosen integer lists to test isDerangement. -}

--1
twoDerangementsOfAThird :: Eq a => [a] -> [a] -> [a] -> Bool
twoDerangementsOfAThird xs ys zs = isDerangement xs zs && isDerangement ys zs --> isDerangement xs ys

--2
isSymmetric :: Eq a =>[a] -> [a] -> Bool
isSymmetric xs ys = isDerangement xs ys --> isDerangement ys xs

--3 
isSortedNotDerangement :: Eq a => Ord a =>[a] -> [a] -> Bool
isSortedNotDerangement xs ys | (length xs /=0 && length ys /=0) = not (isDerangement (sort xs) (sort ys))
                             | otherwise = True -- to not fail in case of isSortedNotDerangement [] []

--4							 
isSelfFalse :: Eq a =>[a] -> Bool
isSelfFalse xs      | (length xs /=0 ) = not (isDerangement xs xs)
                    | otherwise = True -- to not fail in case of isSortedNotDerangement [] []


{- Provide an ordered list of properties by strength using the weakear and stronger definitions. -}

{- we can define a new type to refer to the properties & two functions to print & execute property for the comparison -}
data PropertyType = IsSymmetric | IsSortedNotDerangement | TwoDerangementsOfAThird  | IsSelfFalse deriving (Show, Read)

convertPropertyToString :: PropertyType -> String
convertPropertyToString IsSymmetric             = "IsSymmetric"
convertPropertyToString IsSortedNotDerangement = "IsSortedNotDerangement"
convertPropertyToString TwoDerangementsOfAThird = "TwoDerangementsOfAThird"
convertPropertyToString IsSelfFalse             = "IsSelfFalse"

executeProperty :: PropertyType -> [Int] -> [Int] -> [Int] -> Bool
executeProperty IsSymmetric xs ys _ = isSymmetric xs ys 
executeProperty IsSortedNotDerangement xs ys _ = isSortedNotDerangement xs ys 
executeProperty TwoDerangementsOfAThird xs ys zs = twoDerangementsOfAThird xs ys zs -- we need three lists
executeProperty IsSelfFalse xs _ _ = isSelfFalse xs -- we only need one list

-- If we change the definition of properties comparison functions from lecture2.hs
stronger, weaker :: [Int] -> [Int] -> [Int] -> PropertyType -> PropertyType -> Bool
stronger xs ys zs p q =  executeProperty p xs ys zs --> executeProperty q xs ys zs
weaker   xs ys zs p q = stronger xs ys zs q p

compar :: [Int] -> [Int] -> [Int] -> PropertyType -> PropertyType -> String
compar xs ys zs p q = let pq = stronger xs zs ys p q 
                          qp = stronger xs ys zs q p 
                in 
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"

{- quick sort on properties of at least one list (up to 3 lists) -}
quickSortProps :: [Int] -> [Int] -> [Int] -> [PropertyType] -> [PropertyType]
quickSortProps _ _ _ [] = []
quickSortProps input1 input2 input3 (p:ps) =  do
							   quickSortProps input1 input2 input3 [p' | p' <- ps, stronger input1 input2 input3 p' p]
							++ [p]
							++ quickSortProps input1 input2 input3 [p' | p' <- ps, weaker input1 input2 input3 p' p,
							not(stronger input1 input2 input3 p' p)]

-- Let us consider the following lists- you can test with any dearrangement of lists
list1, list2, list3 :: [Int]
list1 = [1,2,3,4]
list2 = [2,3,1,4]
list3 = [3,4,2,1]

strengthList :: [Int] -> [Int] -> [Int] -> [PropertyType] -> [String]
strengthList _ _ _ [] = []
strengthList xs ys zs ps = [convertPropertyToString p | p <- quickSortProps xs ys zs ps] 

{- Report:
	I picked up four different properties of isDerangement that should hold.
	
	I implemented customized comparison methods for properties (based on strength) & implemented a customized quicksort.
	
	Since my properties are with different number of inputs, I made sure to generalize the compasion.
	
	As a result of running quickCheck for my properties:--------------
	
	 GHCi:
		*Exercise5> quickCheck twoDerangementsOfAThird
		+++ OK, passed 100 tests.

		*Exercise5> quickCheck isSymmetric
		+++ OK, passed 100 tests.
		
		*Exercise5> quickCheck isSortedNotDerangement
		+++ OK, passed 100 tests.
		
		*Exercise5> quickCheck isSelfFalse
		+++ OK, passed 100 tests.
		
	Running the comparison using my customized method resulted into the following:-------------
	
	*Exercise5>  strengthList list1 list2 list3 [IsSymmetric, IsSortedNotDerangement, IsSelfFalse, TwoDerangementsOfAThird]
		["TwoDerangementsOfAThird","IsSelfFalse","IsSortedNotDerangement","IsSymmetric"]
-}