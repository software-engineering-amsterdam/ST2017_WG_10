--Recognizing Permutations - time: 50 minutes
module Exercise4 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{- the following functions are given in lecture2.hs -}
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)
   
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

isTrue :: a -> Bool
isTrue _ = True

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


{----------------------------------------------------------------------}

{- The following function is to check whether two lists are permutation of one another -}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []       = True
isPermutation [] _        = False
isPermutation _ []        = False
isPermutation (p:px) qx   | elem p qx = isPermutation px (delete p qx)
                          | otherwise = False

{----------------------------------------------------------------------}
{- Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation. You may assume that your      
   input lists do not contain duplicates. What does this mean for your testing procedure?
	
	A: that means we've strengthen the pre-condition of isPermutation to gurantee the output of isPermutation holds for post conditions.
	   we're then decreased the test area.
-}


{- properties - relations between two sets (lists) -}

--1
isCommutative :: Ord a => [a] -> [a] -> Bool
isCommutative xs ys = sort xs == sort ys

--2
isAssosiative :: Eq a => Num a => [a] -> [a] -> Bool
isAssosiative xs ys = sum xs == sum ys

--3
isTransitive :: Ord a => [a] ->[a] -> [a] -> Bool
isTransitive xs ys zs = isPermutation xs ys && isPermutation ys zs --> isPermutation xs zs

--4
isSymmetric :: Ord a => [a] -> [a] -> Bool
isSymmetric xs ys  = isPermutation xs ys --> isPermutation ys xs

--5
isReflexive :: Ord a => [a] -> Bool
isReflexive xs = isPermutation xs xs

--6
isCommutativeToItself :: Ord a => [a] -> Bool
isCommutativeToItself xs = isPermutation xs (sort xs)

{---------------------------------------------------------------------------------------}

-- chosen lists for testing

list1, list2 :: [[Int]]
list1 = [[11,0,2,100,92], [2,5,6,4,1], [33,4,5,7,2]]
list2 = [[92,2,100,0,11], [6,5,4,2,1], [4,33,7,2,5]]

testProps :: [Int] -> Bool
testProps xs = (isCommutative xs xs) && (isAssosiative xs xs) && (isTransitive xs xs xs) && (isSymmetric xs xs)


hoareTripleValidation :: (a -> Bool) -> (a -> Bool) -> (Bool -> Bool)-> [a]-> Bool
hoareTripleValidation pre f post = all (\x -> pre x --> post (f x))

{-
	automaticing test using hoare triple
	GHCi:
	*Exercise4> hoareTripleValidation isTrue testProps id [x | x <- list1] 
     True

-}


{- quickCheck
   GHCi:
    quickCheck testProps
	*Exercise4> quickCheck testProps
     +++ OK, passed 100 tests.
-}


{--------------------------------------------------------------------------------------------------
    Provide an ordered list of properties by strength using the weakear and stronger definitions.
---------------------------------------------------------------------------------------------------}

-- given the following functions from lecture2.hs
stronger, weaker :: [a] -> ([a] -> Bool) -> ([a] -> Bool) -> Bool
stronger xs p q =  p xs --> q xs
weaker   xs p q = stronger xs q p

compar :: [a] -> ([a] -> Bool) -> ([a] -> Bool) -> String
compar xs p q = let pq = stronger xs p q 
                    qp = stronger xs q p 
                in 
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"



-- Let us implement the quick sort for properties (consider properties indexed as follows: [0..3])
quicksortProbs :: [Int] -> [Int]
quicksortProbs [] = []  
quicksortProbs (p:px) = 
   quicksortProbs [a | a <- px, elem (compar (head list1) (properties !! a) (properties !! p)) ["stronger","equivalent"]]  
   ++ [p]
   ++ quicksortProbs [a | a <- px, compar (head list1) (properties !! a) (properties !! p) == "weaker" ] 

-- let us define a list of implemented properties, we can test similiar properties (same definition)
properties = [isReflexive, isCommutativeToItself] --[isCommutative, isAssosiative, isSymmetric] -- [0..3]

strengthList :: [Int]
strengthList = quicksortProbs [0..3]

hasNoUniqueElement :: Eq a => [a] -> [a] -> Bool
hasNoUniqueElement xs ys = forall xs (\x -> elem x ys) && forall ys (\y -> elem y xs)


test :: [Int] -> [Int] -> Bool
test a b = (hasNoUniqueElement xs ys) --> isPermutation xs ys
    where
        xs = nub a
        ys = nub b

--nub a returns `a` without duplicates

main = do
print "0)isReflexive      1)isCommutativeToItself:"
print (compar list1 isReflexive isCommutativeToItself)

{-
print "[0,isCommutative, 1,isAssosiative, 2,isSymmetric]"
print (compar' list1 isCommutative isAssosiative)
print (compar2' list1 isCommutative isSymmetric)
print (compar' list1 isAssosiative isSymmetric)
print (compar' list1 isAssosiative isCommutative) -}



{-
If two sequences are permutations of each other, they have to have the same number of elements, and every element
of one must be also in the second. Thankfully, due to the requirement that every element is max. once in the sequences,
we don't have to check how many times is it there, we just need to check if it is present.
If this weren't the case, we could compare two sequences by sorting each of them and then comparing elements
at the same index (this is actually faster: O(n log n) vs. O(n^2), however it's more complicated).

We can test these properties:
a) if the sequences' length is the same
b) if there is no element unique to one of sequences

if these two are satisfied, then isPermutation should work.

b) is stronger than a), proof by contradiction: let's have two lists without duplicates that satisfy b) but not a).
every element of a is in b, therefore len(a) <= len(b), also len(b)<=len(a). This is true only if
the lengths are the same, which is a contradiction.
-}


