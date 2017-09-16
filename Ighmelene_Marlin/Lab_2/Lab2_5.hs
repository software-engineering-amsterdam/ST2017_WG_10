module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 30 min
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement as bs = isPermutation as bs && isIrreflexive as bs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation as bs = isBijective as bs

isIrreflexive :: Eq a => [a] -> [a] -> Bool
isIrreflexive as bs = let r = zip as bs in r == filter (\x -> fst x /= snd x) r

isBijective :: Eq a => [a] -> [a] -> Bool
isBijective as bs = isInjective as bs && inSurjective as bs

isInjective :: Eq a => [a] -> [a] -> Bool
isInjective as bs = as == [y | x <- as, y <- bs, y == x]

inSurjective :: Eq a => [a] -> [a] -> Bool
inSurjective as bs = bs == [x | x <- bs, elem x as]

deran :: Eq a => [a] -> [[a]]
deran xs = filter (\ys -> isDerangement xs ys) (permutations xs)

-- Order properties by strength
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: Eq a => [a] -> [a] -> LabProperty -> LabProperty -> Bool
stronger as bs p q = forall as (\ x -> propertyCheck p as bs --> propertyCheck q as bs)
weaker   as bs p q = stronger as bs q p 

data LabProperty = Injective | Surjective | Bijective | Irreflexive deriving (Show)

propertyCheck :: Eq a => LabProperty -> [a] -> [a] -> Bool
propertyCheck Injective as bs = isInjective as bs
propertyCheck Surjective as bs = inSurjective as bs
propertyCheck Bijective as bs = isBijective as bs
propertyCheck Irreflexive as bs = isIrreflexive as bs

orderByStrength :: Eq a => [a] -> [a] -> [LabProperty] -> [LabProperty]
orderByStrength as bs [] =  []
orderByStrength as bs (p1:ps) =    orderByStrength as bs [p2 | p2 <- ps, stronger as bs p2 p1,not(weaker as bs p2 p1)]
                                ++ [p1]
                                ++ orderByStrength as bs [p2 | p2 <- ps, weaker as bs p2 p1]

-- Automate test
