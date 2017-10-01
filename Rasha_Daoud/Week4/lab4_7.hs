-- -time: 30 minutes
module Lab4_7 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import Lab4_5
import Lab4_6


-- We can use the following functions from lecture 2
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)


{- Test the functions symClos and trClos from the previous exercises. Devise your own test method for this. Try to use random test generation. Define reasonable properties to test. Can you use QuickCheck? How?
-}


{-----------------------------------------1- Random relations generator------------------------------------}

-- First we need to a generator of relation [(a,a)] from a list of a [a]
generateRelFromList :: Eq a => [a] -> Rel a
generateRelFromList [] = []
generateRelFromList [x] = []
generateRelFromList [x,y] = [(x,y)]
generateRelFromList (x:y:xs) = (x,y): generateRelFromList xs

-- Now, we can n generate random relations [(Int, Int)]
generateRel :: IO (Rel Int)
generateRel =  do n <- getRandomInt 100
                  k <- getRandomInt 20 -- can be 0, which means empty relation
                  xs <- getIntL k n
                  return $ nub $ generateRelFromList xs

-- Next, let us generate n relations
generateRels :: Int -> IO [Rel Int]
generateRels 0 = return []
generateRels n = do
                  rel <- generateRel
                  rels <- generateRels (n-1)
                  return (rel:rels)
{-----------------------------------------2- Testing using the random generator--------------------------------}

-- Properties

--1) transative/symmatric closures for empty relation is the empty relation
symmTrEmpty :: Eq a => Ord a => (Rel a) -> Bool
symmTrEmpty r | r == [] = (symClos r == []) && (trClos r == []) 
              | otherwise = True

--2) symmetric closure of a relation should include the relation itself
isSymmIncluded :: Eq a => Ord a => (Rel a) -> Bool
isSymmIncluded r = and [elem x sym | x <- r]
                   where
                    sym = symClos r

--3) transitive closure of a relation should include the relation itself
isTransIncluded :: Eq a => Ord a => (Rel a) -> Bool
isTransIncluded r = and [elem x sym | x <- r]
                   where
                    sym = trClos r

-- Let's modify the testing function from last weeks to take relations & properties on relations as input
testIter :: Int -> (Rel Int -> Bool) -> [Rel Int] -> IO()
testIter n p [] = print (show n ++ " tests passed...")
testIter n p (r:rs) = 
                        if p r then
                         do
                          print ("test passed on:" ++ show r)
                          testIter n p rs
                        else
                          error ("test failed on:" ++ show r)

testRels :: Int -> (Rel Int -> Bool) -> IO()
testRels n p = do 
                      rels <- generateRels n
                      testIter n p rels

-- let's test any of the defined properties for 100 times, on relations generated with our generator 'generateRels'
-- We make type a explicit, let's consider Int

test100Rels :: (Rel Int -> Bool) -> IO()
test100Rels p = testRels 100 p



{-----------------------------------------3- Tesing using QuickCheck--------------------------------------------}

{- Can you use QuickCheck? How?
   yes that is possible. If we specify type a as specific type (e.g. Int)
   We can call quickCheck with any of the above properties on relations, because a list of pairs (of Int) is an instance of Arbitrary.
   
   Therefore not as in lab4_2, we don't need to define an instance of Arbitrary for Rel Int.
   *Now, we're ready to test these properties with quickCheck

-}

-- quickCheck for property p
quickCheckProp :: (Rel Int -> Bool) -> IO()
quickCheckProp p = quickCheck p

{- GHCi:
    
    *Lab4_7> quickCheckProp isTransIncluded
    +++ OK, passed 100 tests.
    *Lab4_7> quickCheckProp symmTrEmpty
    +++ OK, passed 100 tests.
    *Lab4_7> quickCheckProp isSymmIncluded
    +++ OK, passed 100 tests.
-}

-- execute Lab4_7.main to see the results
main = do
        test100Rels symmTrEmpty;
        test100Rels isSymmIncluded;
        test100Rels isTransIncluded;