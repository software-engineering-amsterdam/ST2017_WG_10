-- time: 20 minutes
module Lab6_1
where

import Data.List
import System.Random
import Lecture6

{- we can re-implement the function following the question as follows: -}
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 _ = 1
exM' x 1 z = mod x z
exM' x y z  | even y = multM exm exm z 
            | otherwise = multM x (multM exm exm z) z
           where  exm  = exM' x (div y 2) z

main61 = do
          print ("exM' 10 5 6 = " ++ show (exM' 10 5 6));
          print ("expM 10 5 6 = " ++ show (expM 10 5 6));
          print ("exM' 3 5 7 = " ++ show (exM' 3 5 7));
          print ("expM 3 5 7 = " ++ show (expM 3 5 7));
          print ("exM' 4 13 497 = " ++ show (exM' 4 13 497));
          print ("expM 4 13 497 = " ++ show (expM 4 13 497));
{- 

*Lab6_2> main61
"exM' 10 5 6 = 4"
"expM 10 5 6 = 4"
"exM' 3 5 7 = 5"
"expM 3 5 7 = 5"
"exM' 4 13 497 = 445"
"expM 4 13 497 = 445"

-}