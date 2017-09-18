module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 180 min
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = injective xs ys && surjective xs ys
                  
-- Properties
noDuplicates :: [Int] -> Bool
noDuplicates xs = length (filter (\n -> n > 1) (map (\x -> (length [x1 | x1 <- xs, x1 == x])) xs)) == 0

isSubset :: [Int] -> [Int] -> Bool
isSubset xs ys = xs == [x | x <- xs, elem x ys]

-- each x has exactly one y
injective :: [Int] -> [Int] -> Bool
injective xs ys =  noDuplicates ys && isSubset xs ys

-- each y has at least one x
surjective :: [Int] -> [Int] -> Bool
surjective xs ys = isSubset ys xs

-- Order properties by strength
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: [Int] -> [Int] -> LabProperty -> LabProperty -> Bool
stronger as bs p q = forall as (\ x -> propertyCheck p as bs --> propertyCheck q as bs)
weaker   as bs p q = stronger as bs q p 

data LabProperty = Injective | Surjective | NoDuplicates | Subset deriving (Show)

propertyCheck :: LabProperty -> [Int] -> [Int] -> Bool
propertyCheck Injective xs ys = injective xs ys
propertyCheck Surjective xs ys = surjective xs ys
propertyCheck NoDuplicates xs ys = noDuplicates ys
propertyCheck Subset xs ys = isSubset xs ys

orderByStrength :: [Int] -> [Int] -> [LabProperty] -> [LabProperty]
orderByStrength as bs [] =  []
orderByStrength as bs (p1:ps) = orderByStrength as bs [p2 | p2 <- ps, stronger as bs p2 p1,not(weaker as bs p2 p1)]
                                ++ [p1]
                                ++ orderByStrength as bs [p2 | p2 <- ps, weaker as bs p2 p1]

hasValues :: [Int] -> [Int] -> Bool
hasValues xs ys = length xs * length ys > 0

testInjective :: IO ()
testInjective = verboseCheck (\xs ys -> noDuplicates ys ==> isSubset xs ys ==> injective xs ys)

testSurjective :: IO ()
testSurjective = verboseCheck (\xs ys -> isSubset ys xs ==> surjective xs ys)

testPermutations :: IO ()
testPermutations = verboseCheck (\xs ys -> injective xs ys && surjective xs ys ==> isPermutation xs ys)

-- If empty lists are allowed, the only passed tests are ones on 2 empty lists
testPermutationsNoEmptyLists :: IO ()
testPermutationsNoEmptyLists = verboseCheck (\xs ys -> hasValues xs ys ==> injective xs ys && surjective xs ys ==> isPermutation xs ys)

{----------------------------------------------------------------------}
{- You may assume that your      
   input lists do not contain duplicates. What does this mean for your testing procedure?
	
	A: that means we've strengthened the pre-condition of isPermutation function to gurantee the output of isPermutation holds for post conditions.
	 in other words, we've decreased the test area. 
-}

{- Report:
-- Hardly any tests pass because the properties injective and surjective are not comparable so the preconditions are strong
-- Another problem we experienced with the tests was that the few tests that did pass were of 2 empty lists
-- so we strengthened the preconditions some more with a new property hasValues.
-- That lead to something like 2 tests passing vs 7-9 with empty lists.

-}