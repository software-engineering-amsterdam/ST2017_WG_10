-- -time: 30 minutes
module Lab4_8 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import Lab4_5
import Lab4_6
import Lab4_7


{-
Is there a difference between the symmetric closure of the transitive closure of a relation R and 
the transitive closure of the symmetric closure of R?
Deliverable: If your answer is that these are the same, you should give an argument, if you think these are different you should give an example that illustrates the difference.

-}

-- Let's find out using the following property and an automated testing

isTrueArg :: Eq a => Ord a => (Rel a) -> Bool
isTrueArg r = (symClos $ trClos r) == (trClos $ symClos r)

{- 
  *Lab4_8> isTrueArg [(0,0),(1,0)]
  False
  
  Conclusion: the argument is not correct. Please consult Lab4_8.docx for the structured reporting.
-}


{- we can also generate random relations and test the property on them as follows -}

-- we need to a generator of relation [(a,a)] from a list of a [a]
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
test100Rels :: (Rel Int -> Bool) -> IO()
test100Rels p = testRels 100 p



{-
  
 *Lab4_8> test100Rels isTrueArg
 *** Exception: test failed on:[(0,0),(1,0)]
 CallStack (from HasCallStack):
  error, called at .\Lab4_7.hs:87:27 in main:Lab4_7
  

A lot of other examples are found, but I've chosen the smallest example I've found. 

example1:
 *Lab4_8> test100Rels isTrueArg
 *** Exception: test failed on:[(-2,-3),(9,9),(7,1),(-2,-6),(3,1),(-1,0),(-5,9),(9,0),(8,-3),(-9,-8),(0,-2),(-9,-4),(0,0),(1,0),(-7,2),(7,3),(7,9),(-3,8),(9,-2),(8,4),(4,6)]
 CallStack (from HasCallStack):
  error, called at .\Lab4_7.hs:87:27 in main:Lab4_7

example2:  
 *Lab4_8> test100Rels isTrueArg
 *** Exception: test failed on:[(-17,16),(9,10),(-2,-1),(-20,-7),(0,5),(0,-6),(17,-20),(16,8),(4,-15),(-15,6)]
 CallStack (from HasCallStack):
  error, called at .\Lab4_7.hs:87:27 in main:Lab4_7

example3:
 *Lab4_8> test100Rels isTrueArg
 *** Exception: test failed on:[(19,-15),(0,5),(7,-8),(12,-5),(-8,-3),(-16,19),(16,-2),(2,0),(-19,10),(-11,-14),(-13,14),(-13,-3),(0,19),(-11,3),(14,17),(-10,-13),(-18,14),(-17,-3),(-19,16),(-11,11),(10,-7),(-14,-1),(16,10),(17,-3),(12,8),(12,13),(10,-19),(-19,15),(-1,-14),(1,-13),(7,12),(-4,2),(-19,-17),(-15,-11),(-9,13)]
 CallStack (from HasCallStack):
  error, called at .\Lab4_7.hs:87:27 in main:Lab4_7

-}

main = do
         isTrueArg [(0,0),(1,0)];


{- However, we've noticed that another statement might be true. We implemented the following property: isTrueNewArg-}
 
isTrueNewArg :: Eq a => Ord a => (Rel a) -> Bool
isTrueNewArg r = (trClos $ symClos $ trClos r) == (trClos $ symClos r)
--You can execute command "test100Rels isTrueNewArg" to check correctness.



-- helper functions from lecture2
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