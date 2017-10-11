module Lab6_1 where

-- 30 min
exM' :: Integer -> Integer -> Integer -> Integer
exM' x y z  
  | z == 0    = error ("Divide by 0")
  | y <  0    = error ("Negative exponent")
  | y == 0    = mod 1 z
  | x == 0    = 0
  | y == 1    = mod x z
  | even y    = let m = exM' x (div y 2)  z in mod (m*m) z
  | otherwise = let m = exM' x (pred y)   z in mod (m*x) z