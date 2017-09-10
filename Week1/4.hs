-- Exercise 4 -time: 25 minutes
module Exercise4 where
import Data.List
import Test.QuickCheck    

-- The following three functions are given in the assignment
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
           where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

reversal :: Integer -> Integer
reversal = read . reverse . show


-- First, we need a function to check whether a number is prime and reversable
isReversablePrime :: Integer -> Bool
isReversablePrime n = prime (reversal n)

-- Secondly, we need a function that fetches all prime numbers that are below a (e.g. 1000)
limitedPrimes :: Integer -> [Integer]
limitedPrimes 0 = []
limitedPrimes 1 = []
limitedPrimes a = 2: filter prime ([3..a])

-- Now we need a function to find all primes < 10000 that satisfies that their reversal is also a prime number
getAllPrimesBelowA :: Integer -> [Integer]
getAllPrimesBelowA a = filter (isReversablePrime) (limitedPrimes a)

{- Application in GHCi
 getAllPrimesBelowA 10000 
	[2,3,5,7,11,13,17,31,37,71,73,79,97,101,107,113,131,149,151,157,167,179,181,191,199,311,313,337,347,
	353,359,373,383,389,701,709,727,733,739,743,751,757,761,769,787,797,907,919,929,937,941,953,967,971,
	983,991,1009,1021,1031,1033,1061,1069,1091,1097,1103,1109,1151,1153,1181,1193,1201,1213,1217,1223,
	1229,1231,1237,1249,1259,1279,1283,1301,1321,1381,1399,1409,1429,1439,1453,1471,1487,1499,1511,1523,
	1559,1583,1597,1601,1619,1657,1669,1723,1733,1741,1753,1789,1811,1831,1847,1867,1879,1901,1913,1933,1949,1979,3011,
	3019,3023,3049,3067,3083,3089,3109,3121,3163,3169,3191,3203,3221,3251,3257,3271,3299,3301,3319,3343,3347,3359,3371,
	3373,3389,3391,3407,3433,3463,3467,3469,3511,3527,3541,3571,3583,3613,3643,3697,3719,3733,3767,3803,3821,3851,3853,
	3889,3911,3917,3929,7027,7043,7057,7121,7177,7187,7193,7207,7219,7229,7253,7297,7321,7349,7433,7457,7459,7481,7507,
	7523,7529,7547,7561,7577,7589,7603,7643,7649,7673,7681,7687,7699,7717,7757,7817,7841,7867,7879,7901,7927,7949,7951,
	7963,9001,9011,9013,9029,9041,9103,9127,9133,9161,9173,9209,9221,9227,9241,9257,9293,9341,9349,9403,9421,9437,9439,
	9467,9479,9491,9497,9521,9533,9547,9551,9601,9613,9643,9661,9679,9721,9749,9769,9781,9787,9791,9803,9833,9857,9871,
	9883,9923,9931,9941,9967]
-}


-- We can implement 'doubleRev' to check whether applying double reversal to an n returns n
doubleRev :: Integer -> Bool
doubleRev n = (reversal (reversal n) == n)

{-
    - How would you test this function, by the way?
	 
	 A: by applying the function twice to the same integer using doubleRev. The returning value should be same as the input value.
	 
	 If we use quickCheck to run 'doubleRev' on random integers, it will fail.
     the quickCheck will produce random integers, some could be negative. Negative n is a problem
	 
	  - reversal won't work as well (reverse)
		 *Exercise4> reversal (negate 1)
		 *** Exception: Prelude.read: no parse
		 
	  - quickCheck will fail:  
		*Exercise4> quickCheck doubleRev
		*** Failed! Exception: 'Prelude.read: no parse' (after 4 tests and 2 shrinks): 
		-1
-}

{- We've chosen this solution, because it was the correct answer to the asked question, and the function is general, it can be applied on different  
  limits  -}