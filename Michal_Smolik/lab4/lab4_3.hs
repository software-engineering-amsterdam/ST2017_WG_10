module Lab4_3 where
import SetOrd
import System.Random
import Data.List
import Lab4_2


infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

isc :: Ord a => Set a -> Set a -> Set a
isc (Set xs) ys = list2set [e | e <- xs,  inSet e ys ]

uni :: Ord a => Set a -> Set a -> Set a
uni (Set xs) (Set ys) = list2set (xs ++ ys)

diff :: Ord a => Set a -> Set a -> Set a
diff (Set xs) ys = list2set [x | x <- xs, not (x `inSet` ys)]

--look if everything in union is in either of originals and if every element of originals is in union
testUni :: Ord a => Set a -> Set a -> Bool
testUni (Set xs) (Set ys) = all (\x -> x `inSet` uni (Set xs) (Set ys)) xs &&
                            all (\y -> y `inSet` uni (Set xs) (Set ys)) ys &&
                            all (\i -> elem i xs || elem i ys) is
                                where 
                                    Set(is) = uni (Set xs) (Set ys)
--look if every element represented in both originals is in intersection and if every element of intersection is in both originals
testIsc :: Ord a => Set a -> Set a -> Bool
testIsc (Set xs) (Set ys) = all (\i -> i `inSet` (Set xs) && i `inSet` (Set ys)) is &&
                            all (\x -> x `inSet` (Set ys) --> x `inSet` (Set is)) xs where
                                Set(is) = isc (Set xs) (Set ys)
--look if every element of difference is only in first set and every element of first one that's in both is not in the diff.
testDiff :: Ord a => Set a -> Set a -> Bool
testDiff (Set xs) (Set ys) = all (\i -> i `inSet` (Set xs) && not (i `inSet` (Set ys))) is &&
                             all (\x -> x `inSet` (Set ys) --> not (x `inSet` (Set is))) xs where
                                Set(is) = diff (Set xs) (Set ys)

runTests :: Int -> Int -> IO()
runTests k n = do
            if k == n then print(show n ++ " tests passed") else do
                s1 <- genRandomSet
                s2 <- genRandomSet
                if (not (testUni s1 s2)) then error("Union failed for sets "++show s1++" and "++show s2++", supposed union is "++ show (uni s1 s2))
                else do 
                if (not (testIsc s1 s2)) then error("Intersection failed for sets "++show s1++" and "++show s2++", supposed intersection is "++ show (isc s1 s2))
                else do
                if (not (testUni s1 s2)) then error("Difference failed for sets "++show s1++" and "++show s2++", supposed difference is "++ show (diff s1 s2))
                else do
                    runTests (k+1) n

runTestsQuickCheck :: Int -> Int -> IO()
runTestsQuickCheck k n = do
            if k == n then print(show n ++ " tests passed") else do
                s1 <- genArbitrarySet
                s2 <- genArbitrarySet
                if (not (testUni s1 s2)) then error("Union failed for sets "++show s1++" and "++show s2++", supposed union is "++ show (uni s1 s2))
                else do 
                if (not (testIsc s1 s2)) then error("Intersection failed for sets "++show s1++" and "++show s2++", supposed intersection is "++ show (isc s1 s2))
                else do
                if (not (testUni s1 s2)) then error("Difference failed for sets "++show s1++" and "++show s2++", supposed difference is "++ show (diff s1 s2))
                else do
                    runTests (k+1) n


--all tests pass 100 checks on both generators, so we can guess that generators and functions are implemented correctly enough
--so that the required properties hold. These tests however offer little assurance as for randomness of the generated sets.
main = do
    runTests 0 100
    runTestsQuickCheck 0 100

-- time spent: 30 min