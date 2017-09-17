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

rotate :: Char -> Char
rotate c 
    |elem c ['a'..'z'] = chr.(+97).(`mod` 26).(+ (-84)).ord $ c
    |elem c ['A'..'Z'] = chr.(+65).(`mod` 26).(+ (-52)).ord $ c
    |otherwise = c
rot13 :: String -> String
rot13 xs = map (rotate) xs
{-
Specification: 
Input: string containing any characters
Output: string, where every upper- or lowercase character has been rotated by 13 places, and substituted by
    a character in the same case (uppercase will produce uppercase, and lowercase will produce lowercase).
    Non-english letters will not be altered in any way.
-}