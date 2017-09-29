module Lab4_5 where

import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd
import Lab4_3

-- 30 min
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos rs = do
              let frs       = flipRel rs
              let a         = list2set rs
              let b         = list2set frs
              let d         = setDifference a b
              let i         = setIntersection a b
              let (Set sc)  = setUnion d i
              sc

flipPair :: Eq a => (a,a) -> (a,a)
flipPair r = let (a,b) = r in (b,a)

flipRel :: Eq a => Rel a -> Rel a
flipRel rs = map flipPair rs

flipRelExists :: Eq a => (a,a) -> Rel a -> Bool
flipRelExists r rs = let (a,b) = r in elem (b,a) rs