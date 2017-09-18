module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 20 min
rot13 :: [Char] -> [Char]
rot13 s = rotString s 13

rotString :: [Char] -> Int -> [Char]
rotString s n = [rotChar c n | c <- s]

rotChar :: Char -> Int -> Char
rotChar c n | (between (ord c) 65 90)   = chr (65 + (mod ((n + (ord c)) - 65) 26))
            | (between (ord c) 97 122)  = chr (97 + (mod ((n + (ord c)) - 97) 26))
            | otherwise = c
            
between :: Int -> Int -> Int -> Bool
between n x y = if (x <= n && n <= y) then True else False

-- 45 min
isIrreflexive :: Eq a => [(a,a)] -> Bool
isIrreflexive r = length (filter (\x -> fst x == snd x) r) == 0

isNontransitive :: Eq a => [(a,a)] -> Bool
isNontransitive r = length [z | x <- r, y <- r, z <- r, snd x == fst y, (fst x,snd y) == z] == 0