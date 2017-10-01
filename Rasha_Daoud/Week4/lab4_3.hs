-- -time: 40 minutes
module Lab4_3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd
import Lab4_2

{- Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs.
   Next, use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck. -}

setToList :: Set Int -> [Int]
setToList (Set []) = []
setToList (Set xs) = xs

--1) returns a set of intersection of two sets 
getIntersection :: (Ord a) => Set a -> Set a -> Set a 
getIntersection _ (Set [])  =  Set []
getIntersection (Set []) _  =  Set []
getIntersection (Set (x:xs)) (Set ys)  | elem x ys = insertSet x $ getIntersection (Set xs) (Set ys)
                                       | otherwise = getIntersection (Set xs) (Set ys)

{- GHCi:

    *Lab4_3> getIntersection (Set [1,2,3,4]) (Set [2,3,5,6,4,2,1])
    {1,2,3,4}
-}

--2) returns a union of two sets
getUnion :: (Ord a) => Set a -> Set a -> Set a 
getUnion s1 (Set [])  =  s1
getUnion (Set []) s2  =  s2
getUnion (Set (x:xs)) (Set ys)         | elem x ys = getUnion (Set xs) (Set ys)
                                       | otherwise = insertSet x $ getUnion (Set xs) (Set ys)
{- GHCi:

  *Lab4_3> getUnion (Set [1,2,3]) (Set [2,4,5])
  {1,2,3,4,5}
-}

--3) returns a set that has the difference between two sets (non-shared elementes between two sets).
getDifference :: (Ord a) => Set a -> Set a -> Set a 
getDifference s1 (Set [])  =  (Set []) 
getDifference (Set []) s2  =  (Set []) 
getDifference (Set (x:xs)) (Set ys)    | elem x ys = getDifference (Set xs) (Set ys)
                                       | otherwise = insertSet x $ getDifference (Set xs) (Set ys)
{- GHCi:

   *Lab4_3> getDifference (Set [1,2,3]) (Set [2,4,5])
   {1,3}
-}

{---------------------------------------Testing properties----------------------------------------}

-- First we need to implement some testable properties to check the correctness of the functions above

--1) getIntersection should return a set of elements, each of these elements should exist in both original sets
checkIntersection :: Set Int -> Set Int -> Bool
checkIntersection s1@(Set xs) s2@(Set ys) = and [elem x xs && elem x ys | x <- zs]
                          where (Set zs) = getIntersection s1 s2

--2) a union of two sets sguld return the same set, as if we union the two [Int] from both sets, then we apply the given list2set function.
checkUnion :: Set Int -> Set Int -> Bool
checkUnion s1@(Set xs) s2@(Set ys) = list2set (union xs ys) == (getUnion s1 s2)

--3) a difference between two sets, is same as the difference of converting to set, the difference between their [Int]
checkDiff1 :: Set Int -> Set Int -> Bool
checkDiff1 s1@(Set xs) s2@(Set ys) = list2set (xs \\ ys) == (getDifference s1 s2)

--4) each element in the difference of two sets should exist in one of the original sets ONLY
checkDiff2 :: Set Int -> Set Int -> Bool
checkDiff2 s1@(Set xs) s2@(Set ys) = and [((elem z xs) && (not $ elem z ys)) || ((not $ elem z xs) && (elem z ys))| z <- zs]
                                      where (Set zs) = getDifference s1 s2


{-----------------------------------1- Testing using our own generator-----------------------------------}

-- Let's modify testing functions from lab4_2 to have different type of properties (different definition)
testIter_ :: Int -> (Set Int -> Set Int -> Bool) -> [Set Int] -> IO()
testIter_ n p [] = print (show n ++ " tests passed...")
testIter_ n p (s1:s2:ss) = 
                        if p s1 s2 then
                         do
                          print ("test passed on:" ++ show s1 ++ show s2)
                          testIter_ n p ss
                        else
                          error ("test failed on:" ++ show s1 ++ show s2)

testSets_ :: Int -> (Set Int -> Set Int -> Bool) -> IO()
testSets_ n p = do 
                      sets <- generateRandomSets n
                      testIter_ n p sets

-- let's test any of the defined properties for 100 times, on sets generated with our generator generateRandomSets
test100Sets_ :: (Set Int -> Set Int -> Bool) -> IO()
test100Sets_ p = testSets_ 100 p


{-----------------------------------2- Testing using QuickCheck-----------------------------------}

-- Now, we're ready to test these properties with quickCheck

-- quickCheck for property p
quickCheckProp :: (Set Int -> Set Int -> Bool) -> IO()
quickCheckProp p = quickCheck p


{- GHCi: quickcheck results

   *Lab4_3> quickCheckProp checkIntersection
   +++ OK, passed 100 tests.
   
   *Lab4_3> quickCheckProp checkUnion
   +++ OK, passed 100 tests.
   
   *Lab4_3> quickCheckProp checkDiff1
   +++ OK, passed 100 tests.
   
   *Lab4_3> quickCheckProp checkDiff2
   +++ OK, passed 100 tests.
-}


-- execute Lab4_3.main to see result from the automated testing with our random generator
main = do
         test100Sets_ checkIntersection;
         test100Sets_ checkUnion;
         test100Sets_ checkDiff1;
         test100Sets_ checkDiff2;