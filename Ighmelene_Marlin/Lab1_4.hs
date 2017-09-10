module Lab1 where
import Data.List
import Data.Numbers.Primes
import Test.QuickCheck

-- 120 min
-- I would reverse all the elements in the list and see if i get the same list back.
-- But maybe I'd lose some values because the reversal is > n?
-- Get the highest number and check if the reversal is > n
-- If so, increase n to include the reversal of the largest reversable prime

reversal :: Int -> Int
reversal = read . reverse . show

getReversablePrimesUnder :: Int -> [Int]
getReversablePrimesUnder n = [x | x <- (takeWhile (< n) primes), x >= 10, isPrime (reversal x)]

listReversal :: [Int] -> [Int]
listReversal xs = sort [reversal x | x <- xs]

p :: Int -> Bool
p n = if(n >= x) 
        then xs == listReversal (xs)
        else p x
        where 
              xs  = getReversablePrimesUnder n
              rxs = listReversal xs
              x   = if(length rxs > 0) then 1 + (last rxs) else 0