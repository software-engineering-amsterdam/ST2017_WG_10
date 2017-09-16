module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 90 min
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation (x:xs) ys = length xs == length ys &&
                          length ([x]++(filter (\a -> a == x) xs)) == length (filter (\b -> b == x) ys)
                          
-- Properties      
injective :: Eq a => [a] -> [a] -> Bool
injective as bs = as == [y | x <- as, y <- bs, y == x]

surjective :: Eq a => [a] -> [a] -> Bool
surjective as bs = bs == [x | x <- bs, elem x as]

bijective :: Eq a => [a] -> [a] -> Bool
bijective as bs = injective as bs && surjective as bs

-- Order properties by strength
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: Eq a => [a] -> [a] -> LabProperty -> LabProperty -> Bool
stronger as bs p q = forall as (\ x -> propertyCheck p as bs --> propertyCheck q as bs)
weaker   as bs p q = stronger as bs q p 

data LabProperty = Injective | Surjective | Bijective deriving (Show)

propertyCheck :: Eq a => LabProperty -> [a] -> [a] -> Bool
propertyCheck Injective as bs = injective as bs
propertyCheck Surjective as bs = surjective as bs
propertyCheck Bijective as bs = bijective as bs

orderByStrength :: Eq a => [a] -> [a] -> [LabProperty] -> [LabProperty]
orderByStrength as bs [] =  []
orderByStrength as bs (p1:ps) =    orderByStrength as bs [p2 | p2 <- ps, stronger as bs p2 p1,not(weaker as bs p2 p1)]
                                ++ [p1]
                                ++ orderByStrength as bs [p2 | p2 <- ps, weaker as bs p2 p1]

-- Automate test
