module Lab4_7 where

import Data.List
import System.Random
import Test.QuickCheck
import Lab4_5
import Lab4_6

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

genRel :: IO(Rel Int)
genRel = do
    xs <- generate (arbitrary :: Gen [(Int,Int)])
    return xs

isSymmetric :: Eq a => (Rel a) -> Bool
isSymmetric r = all (\(x,y) -> (y,x) `elem` r) r

--every element of small must be in big
isSuperSet :: Eq a => (Rel a) -> (Rel a) -> Bool
isSuperSet big small = all (\x -> x `elem` big) small

testSymClos :: Int -> Int -> IO()
testSymClos k n = do
    if k == n then print(show n ++" tests passed")
        else do
            r <- genRel
            if not (isSymmetric (symClos r)) || not (symClos r `isSuperSet` r) || not (isSmallestPossible (symClos r) r isSymmetric)
                then error("Failed for " ++ show r ++ ", produced relation "++ show (symClos r)++ " is not correct closure")
                else do
                    testSymClos (k+1) n
--transitive closure by list comprehension
isTransitive :: Eq a => (Rel a) -> Bool
isTransitive r =  all (\x -> x `elem` r) [(x,b) | (x,y) <- r, (a,b) <-r, (y == a)]

--removes element at index k
dropAt :: Int -> [a] -> [a]
dropAt k xs = fst p ++ tail (snd p) where
    p = splitAt k xs

--looks if the property p doesn't hold for every smaller subset of b. (if subset of b is not superset of s, then we don't care about the property)
--basically, if answer is false, we can remove element of b to still have b be superset of s and have the property p hold.
isSmallestPossible :: Eq a => (Rel a) -> (Rel a) -> ((Rel a) -> Bool) -> Bool
isSmallestPossible b s p = all (\x -> (x `isSuperSet` s --> not (p x))) [dropAt k b | k <- [0..((length b)-1)]]

testTrClos :: Int -> Int -> IO()
testTrClos k n = do
    if k == n then print(show n ++" tests passed")
        else do
            r <- genRel
            if not (isTransitive (trClos r)) || not (trClos r `isSuperSet` r) || not (isSmallestPossible (trClos r) r isTransitive)
                then error("Failed for " ++ show r ++ ", produced relation "++ show (trClos r)++ " is not correct closure")
                else do
                    testTrClos (k+1) n

main = do 
    testSymClos 0 100
    testTrClos  0 100

{-
Tests passed for both closures. Testing of the transitive closure is not the best, as we are using another implementation
of the function to test transitivity. Also the test for smallest superset might not be the best, as it can theoretically
confirm a set that's not the smallest possible with such property. For this, there must be a smaller set with this property,
which is not a subset of the tested one. We assume that this is unlikely to happen and ignore this problem, because
finding such smaller superset would be another way of solving the problem, not testing it.

Tested properties are:
    Symmetricity (on symClos)
    Transitivity (on trClos)
    Being a superset of original relation
    Being the smallest possible superset with this property.

time: 30 min
-}