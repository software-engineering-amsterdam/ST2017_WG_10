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

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = ((length xs) == (length ys)) && forall xs (\x -> elem x ys)

{-
If two sequences are permutations of each other, they have to have the same number of elements, and every element
of one must be also in the second. Thankfully, due to the requirement that every element is max. once in the sequences,
we don't have to check how many times is it there, we just need to check if it is present.
If this weren't the case, we could compare two sequences by sorting each of them and then comparing elements
at the same index (this is actually faster: O(n log n) vs. O(n^2), however it's more complicated).

We can test these properties:
a) if the sequences' length is the same
b) if there is no element unique to one of sequences

if these two are satisfied, then isPermutation should work.

b) is stronger than a), proof by contradiction: let's have two lists without duplicates that satisfy b) but not a).
every element of a is in b, therefore len(a) <= len(b), also len(b)<=len(a). This is true only if
the lengths are the same, which is a contradiction.
-}

hasNoUniqueElement :: Eq a => [a] -> [a] -> Bool
hasNoUniqueElement xs ys = forall xs (\x -> elem x ys) && forall ys (\y -> elem y xs)

test :: [Int] -> [Int] -> Bool
test a b = (hasNoUniqueElement xs ys) --> isPermutation xs ys
    where
        xs = nub a
        ys = nub b
--nub a returns `a` without duplicates
