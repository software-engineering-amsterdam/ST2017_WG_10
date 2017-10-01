-- -time: 10 minutes
module Lab4_5 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4

type Rel a = [(a,a)]

{- symClos gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs. E.g., symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]. -}

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos (x:xs) = if (fst x == snd x) then (fst x,snd x): symClos xs
                 else (fst x,snd x):(snd x,fst x): symClos xs

{- CHGi:

      *Lab4_5> symClos [(1,2),(2,3),(3,4)]
      [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
	  
	  *Lab4_5> symClos [(2,2), (1,3)]
      [(2,2),(1,3),(3,1)]
-}
