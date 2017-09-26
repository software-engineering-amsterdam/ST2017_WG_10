-- -time: 30 minutes 
module Lab4_6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import Lab4_5

infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

{- trClos gives the transitive closure of a relation, represented as an ordered list of pairs. E.g., trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
-}

-- we need a helper function
transitive :: Eq a =>  Ord a => Rel a -> Rel a
transitive r = nub $ r ++ (r @@ r)

trClos :: Ord a => Rel a -> Rel a
trClos r = if (transitive r == r) then sort $ transitive r else sort $ trClos $ transitive r

{- GHCi:

   *Lab4_6> trClos [(1,2),(2,3),(3,4)]
   [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
-}