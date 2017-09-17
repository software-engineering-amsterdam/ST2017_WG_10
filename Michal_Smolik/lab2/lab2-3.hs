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

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

first, second, third :: Int -> Bool
first x = ((even x) && (x > 3))
second x = ((even x )|| (x > 3))
third x = ((even x) && (x > 3)) || (even x)

{-
for every pair: stronger [(-10) .. 10] _ _
first > second
first > third
third > second

third = even

hierarchy as follows:
Strongest
((even x) && (x > 3))
((even x) && (x > 3)) || (even x)    (equals `even x`)
((even x )|| (x > 3))
weakest
-}