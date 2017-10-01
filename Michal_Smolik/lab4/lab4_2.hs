module Lab4_2 where
import SetOrd
import System.Random
import Data.List
import Test.QuickCheck


getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (-n,n))

--generates list of length l with complexity of c
--complexity is maximum allowed depth of a formula, if we were to interpret it as an parse tree
genListOfInts :: Int -> Int -> IO [Int]
genListOfInts 0 _= return []
genListOfInts l max = do
            x <- (getRandomInt max)
            xs <- genListOfInts (l-1) max
            return (x:xs)

genRandomSet :: IO(Set Int)
genRandomSet = do
    l <- getRandomInt 100
    xs <- genListOfInts (abs l) 1000
    return (list2set xs)
genArbitrarySet :: IO(Set Int)
genArbitrarySet = do
    xs <- generate (arbitrary :: Gen [Int])
    return (list2set xs)

--time spent: 10 min