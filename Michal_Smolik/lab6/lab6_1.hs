module Lab6_1 where


exM :: Integer -> Integer -> Integer -> Integer
exM x y n
    | y == 1 = x `rem` n
    | y `rem` 2 == 0 = (exM (x^2) (y `div` 2) n) `rem` n
    | otherwise = x*(exM (x^2) ((y - 1) `div` 2) n) `rem` n
