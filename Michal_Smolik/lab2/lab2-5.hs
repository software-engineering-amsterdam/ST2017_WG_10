module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] (y:ys) = False
isDerangement (x:xs) [] = False
isDerangement (x:xs) (y:ys) = not (x == y) && isDerangement xs ys

deran :: Int -> [[Int]]
deran n = filter (\xs -> isDerangement [0..(n-1)] xs) $ permutations [0..(n-1)]

{-
We can test these properties:
a) if the sequences' length is the same
b) if there is no element unique to one of sequences
c) if there is no element with the same value on same place in both sequences 

if these three are satisfied, then the sequences are a derangement.

same as with permutations, b) is stronger than a), proof by contradiction:
let's have two lists without duplicates that satisfy b) but not a).
every element of a is in b, therefore len(a) <= len(b), also len(b)<=len(a). This is true only if
the lengths are the same, which is a contradiction.

c) however is not comparable, since, for example [1] [2] satisfy c, but not b and [1,2,3] and [3,2,1]
satisfies b, but not c.
-}
