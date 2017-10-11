module Lab6_3 where

import Data.List
import Lecture6

-- 5 min
myComposites :: [Integer]
myComposites = filter (not.prime) [2..]