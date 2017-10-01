module Lab4_6 where
import Data.List
import Lab4_5


infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos xs = iter xs xs

iter :: Ord a => Rel a -> Rel a -> Rel a
iter xs is = if length (next) == length(is) then is else
                iter xs next
                where
                    next = nub(xs ++ (is @@ xs))
--time: 1 min