-- Special Pythagorean triplet -time: 15 mins
import Data.List
import Test.QuickCheck

-- The following two functions are given in previous assignments
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
           where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

{-  https://projecteuler.net/problem=9
    A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
    a2 + b2 = c2
    For example, 32 + 42 = 9 + 16 = 25 = 52.
    There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    Find the product abc.
-}

-- Wrtie a function to find the product.
{- important facts:
   - We know that there is one solution, therefore we can use head on the solution list.
   - a < b < c:
     NO need to try to pick up b from a list that starts by 1 but ends by (c-1), because b <b
     Same for a, we can use list generator starting from 1 and ending by (b-1), because a <b.
-}
findProductPythagoreanT1000 :: Integer
findProductPythagoreanT1000 = head [a*b*c | c <- [1..1000], b <- [1..(c-1)], a <- [1..(b-1)],
                 (a^2 + b^2 == c^2) && (a + b + c == 1000) ]
{- GHCi:
    *Main> findProductPythagoreanT1000
    31875000
	
  **for the upper value of the list generation of c, we can also find out the correct limit. For now I used 1000 because it can't be more
	a + b + c = 1000 (one can be 1000, the rest can be 0). BUT as you can see 0^2 + 0^2 != 1000^2.
	So we could have used smaller value than 1000 in generating c options.
	TODO- find value based on facts
-}
