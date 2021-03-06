module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 60 min
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: [Int] -> LabProperty -> LabProperty -> Bool
stronger xs p q = forall xs (\ x -> propertyCheck p x --> propertyCheck q x)
weaker   xs p q = stronger xs q p 

data LabProperty = Property1 | Property2 | Property3  | Property4 deriving (Show)

showProperty :: LabProperty -> [Char]
showProperty Property1 = "(\\ x -> even x && x > 3)"
showProperty Property2 = "even"
showProperty Property3 = "(\\ x -> even x || x > 3)"
showProperty Property4 = "(\\ x -> (even x && x > 3) || even x)"

property1,property2,property3,property4 :: Int -> Bool 
property1 a = (\ x -> even x && x > 3) a
property2 a = even a
property3 a = (\ x -> even x || x > 3) a
property4 a = (\ x -> (even x && x > 3) || even x) a

propertyCheck :: LabProperty -> Int -> Bool
propertyCheck Property1 a = property1 a
propertyCheck Property2 a = property2 a
propertyCheck Property3 a = property3 a
propertyCheck Property4 a = property4 a

orderByStrength :: [Int] -> [LabProperty] -> [[Char]]
orderByStrength xs [] = []
orderByStrength xs ps = let o = (getOrder xs ps) in [showProperty x | x <- o]
                                
getOrder :: [Int] -> [LabProperty] -> [LabProperty]
getOrder xs [] =  []
getOrder xs (p1:ps) =  do
                          getOrder xs [p2 | p2 <- ps, stronger xs p2 p1,not(weaker xs p2 p1)]
                          ++ [p1]
                          ++ getOrder xs [p2 | p2 <- ps, weaker xs p2 p1]

