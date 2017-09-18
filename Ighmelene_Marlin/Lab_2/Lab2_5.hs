module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 90 min
isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = isPermutation xs ys && irreflexive xs ys

isPermutation :: [Int] -> [Int] -> Bool
isPermutation xs ys = injective xs ys && surjective xs ys
                  
-- Properties
irreflexive :: [Int] -> [Int] -> Bool
irreflexive xs ys = let r = zip xs ys in r == filter (\x -> fst x /= snd x) r

-- precondition for injective and surjective
isSubset :: [Int] -> [Int] -> Bool
isSubset xs ys = xs == [x | x <- xs, elem x ys]

-- each x has exactly one y
noDuplicates :: [Int] -> Bool
noDuplicates xs = length (filter (\n -> n > 1) (map (\x -> (length [x1 | x1 <- xs, x1 == x])) xs)) == 0

-- each x has exactly one y
injective :: [Int] -> [Int] -> Bool
injective xs ys =  noDuplicates ys && isSubset xs ys

-- each y has at least one x
surjective :: [Int] -> [Int] -> Bool
surjective xs ys = isSubset ys xs

-- no element is in the same position in both lists
uniquePosition :: [Int] -> [Int] -> Bool
uniquePosition xs ys = length (intersect (zip [1..] xs) (zip [1..] ys)) == 0

deran :: [Int] -> [[Int]]
deran xs = filter (\ys -> isDerangement xs ys) (permutations xs)

-- Order properties by strength
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: [Int] -> [Int] -> LabProperty -> LabProperty -> Bool
stronger xs ys p q = forall xs (\ x -> propertyCheck p xs ys --> propertyCheck q xs ys)
weaker   xs ys p q = stronger xs ys q p 

data LabProperty = Injective | Surjective | NoDuplicates | Subset | Irreflexive | UniquePositions deriving (Show)

propertyCheck :: LabProperty -> [Int] -> [Int] -> Bool
propertyCheck Injective xs ys = injective xs ys
propertyCheck Surjective xs ys = surjective xs ys
propertyCheck NoDuplicates xs ys = noDuplicates ys
propertyCheck Subset xs ys = isSubset xs ys
propertyCheck Irreflexive xs ys = irreflexive xs ys
propertyCheck UniquePositions xs ys = uniquePosition xs ys

orderByStrength :: [Int] -> [Int] -> [LabProperty] -> [LabProperty]
orderByStrength xs ys [] =  []
orderByStrength xs ys (p1:ps) =    orderByStrength xs ys [p2 | p2 <- ps, stronger xs ys p2 p1,not(weaker xs ys p2 p1)]
                                ++ [p1]
                                ++ orderByStrength xs ys [p2 | p2 <- ps, weaker xs ys p2 p1]

hasValues :: [Int] -> [Int] -> Bool
hasValues xs ys = length xs * length ys > 0

-- Tests
testInjective :: IO ()
testInjective = verboseCheck (\xs ys -> noDuplicates ys ==> isSubset xs ys ==> injective xs ys)

testSurjective :: IO ()
testSurjective = verboseCheck (\xs ys -> isSubset ys xs ==> surjective xs ys)

testPermutations :: IO ()
testPermutations = verboseCheck (\xs ys -> injective xs ys && surjective xs ys ==> isPermutation xs ys)

testIrreflexive :: IO ()
testIrreflexive = verboseCheck (\xs ys -> uniquePosition xs ys ==> irreflexive xs ys)