module Lab4_5 where
import Data.List

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos xs = deduplicate (xs ++ [(y,x)| (x,y) <- xs])
        where 
            deduplicate :: Ord a => [a] -> [a]
            deduplicate = map head . group . sort
 -- time 1 min