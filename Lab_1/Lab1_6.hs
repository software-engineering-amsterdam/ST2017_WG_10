module Lab1 where
import Data.List
import Data.Numbers.Primes
import Test.QuickCheck

-- 45 min
-- The smallest counter example is 30031

primeProduct :: Int -> Int
primeProduct n = product ( take n primes )

conjecture :: [Int]
conjecture = takeWhile (isPrime) [sum [1, primeProduct x] | x <- [1..]]

refute :: Int
refute = 1 + primeProduct (succ x) 
          where x = length conjecture