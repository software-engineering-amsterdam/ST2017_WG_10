module Lab4_6 where

import Data.List
import System.Random
import Test.QuickCheck    
import Lab4_3
import Lab4_5

-- 45 min
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos rs = let rs' = (sort.nub) (rs ++ (rs @@ rs)) in if(rs' == rs) then rs' else trClos rs'