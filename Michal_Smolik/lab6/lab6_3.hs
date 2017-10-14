module Lab6_3 where

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Int]
primes = 2 : filter prime [3..] 

composites = filter (not.prime) [2..]