-- -time: 40 minutes
module Lab4_2 where

import Data.List
import System.Random
import Test.QuickCheck
import Data.List.Split
import Control.Monad
import Lecture4
import SetOrd

{- Implement a random data generator for the datatype Set Int, where Set is as defined in SetOrd.hs. First do this from scratch, next give a version that uses QuickCheck to random test this datatype. -}


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

generateRandomSet :: IO (Set Int)
generateRandomSet =  do
                          k <- getRandomInt 100 -- k can be 0, which means empty list as a result of applying getIntL -> empty set
                          n <- getRandomInt 100
                          xs <- getIntL k n
                          return $ list2set xs
{-
    *Lab4_2> generateRandomSet
    {0}
    *Lab4_2> generateRandomSet
    { -32,-30,-28,-27,-26,-23,-17,-16,-14,-13,-10,-7,-6,-4,0,1,3,5,11,13,14,16,18,25,26,28,30,31,33}
    *Lab4_2> generateRandomSet
    { -9,-8,-7,-6,-5,-4,-3,-1,0,1,2,3,4,5,6,7,8,9}
    *Lab4_2> generateRandomSet
    { -38,-37,-24,-19,-15,-14,-12,-10,-9,-7,0,7,10,11,13,31,33,40}
    *Lab4_2> generateRandomSet
    { -48,7,10,27,51}
    *Lab4_2> generateRandomSet
    { -20,-19,-18,-17,-16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-1,0,1,2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20}

-}

-- We will need the following function to generate n number of sets, for the testing.
generateRandomSets :: Int -> IO [Set Int]
generateRandomSets 0 = return [] -- base case
generateRandomSets n = do s <- generateRandomSet
                          -- we call function recursively on n-1 until n =0 then we execute the base-case
                          ss <- generateRandomSets (n -1) 
                          return (s:ss)

{-----------------------------------------QuickCheck testing--------------------------------------}
{- First, we can use quickCheck to test some properties of the datatype Set an-arbitrary-instance-for-a-recursive-datatype
   we can declare an instance of Arbitrary for the datatype, and generate arbitrary data as follows:
   
   Instance inspired by https://stackoverflow.com/questions/36055669/how-do-i-create-an-arbitrary-instance-for-a-recursive-datatype
-}

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
 arbitrary = sized $ \n ->
    do k <- choose (1,n)
       liftM list2set $ sequence [arbitrary | _ <- [1..k] ]

-- We can now define three properties for the datatype Set a as follows:

--1)
testProp1 :: Set Int -> Bool
testProp1 set@(Set xs) = set == list2set xs

--2)
testProp2 :: Set Int -> Bool
testProp2 set@(Set (x:xs)) = not $ elem x xss
                           where (Set xss) = deleteSet x set

--3)
testProp3 :: Set Int -> Bool
testProp3 (Set xs) = isEmpty $ list2set (drop (length xs) xs)
          

-- Now, we're ready to test these properties with quickCheck

--qc1) quickCheck for first property
quickCheckProp1 :: IO()
quickCheckProp1 = quickCheck testProp1

--qc2) quickCheck for second property
quickCheckProp2 :: IO()
quickCheckProp2 = quickCheck testProp2

--qc3) quickCheck for third property
quickCheckProp3 :: IO()
quickCheckProp3 = quickCheck testProp3

{-  *Lab4_2> quickCheckProp1
    +++ OK, passed 100 tests.
    
	*Lab4_2> quickCheckProp2
    +++ OK, passed 100 tests.
	
	*Lab4_2> quickCheckProp3
    +++ OK, passed 100 tests.
-}


{-----------------------------------Random test generator - from scratch-----------------------------------}

-- Let's modify the testing function from last weeks to take sets & properties on sets as input
testIter :: Int -> (Set Int -> Bool) -> [Set Int] -> IO()
testIter n p [] = print (show n ++ " tests passed...")
testIter n p (s:ss) = 
                        if p s then
                         do
                          print ("test passed on:" ++ show s)
                          testIter n p ss
                        else
                          error ("test failed on:" ++ show s)

testSets :: Int -> (Set Int -> Bool) -> IO()
testSets n p = do 
                      sets <- generateRandomSets n
                      testIter n p sets

-- let's test any of the defined properties for 100 times, on sets generated with our generator generateRandomSets
test100Sets :: (Set Int -> Bool) -> IO()
test100Sets p = testSets 100 p

-- execute main to see result
main = do
        test100Sets testProp1;
        test100Sets testProp2;
        test100Sets testProp3;
