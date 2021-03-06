module Lab3_2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n+1))

--generates list of length l with complexity of c
--complexity is maximum allowed depth of a formula, if we were to interpret it as an parse tree
genListOfForms :: Int -> Int -> IO [Form]
genListOfForms 0 _= return []
genListOfForms l c = do
            f <- generateFormula c
            fs <- genListOfForms (l-1) c
            return (f:fs)

--generates formula of complexity c
generateFormula :: Int -> IO Form
generateFormula c = do
        p <- getRandomInt 4
        if c == 0 then return (Prop p) else do
            x <- getRandomInt 5
            l <- getRandomInt 5
            f1 <- generateFormula (c-1)
            f2 <- generateFormula (c-1)

            xs <- genListOfForms l (c-1)
            case x of 
                1 -> do return (Prop p)
                2 -> do return (Neg f1)
                3 -> do return (Cnj xs)
                4 -> do return (Dsj xs)
                5 -> do return (Impl f1 f2)
                otherwise -> do return (Equiv f1 f2)

--test method is to see if correct formula generated by our (hopefully correct) generator will yield same formula, after being 
--converted to string and parsed back into `form` type.
testPositive :: Int -> Int -> IO()
testPositive n k = do
            if n == k then print ((show n) ++ " tests passed") else do
                f <- generateFormula 2
                let s = show f
                let parsed = head(parse s)
                if (f == parsed) then testPositive n (k+1) else do
                     error("Failed, formula "++ (show f) ++ " parsed as " ++ (show parsed))

wrongFormulas = ["1 *(1 2)", "==> 1 -1", "(3 ==> 4 4)", "*1", "+(1 (1 <=> -2) *)", "-*"]

testWrongForms :: IO Bool
testWrongForms = return (all (\x -> (parse x) == []) wrongFormulas)

main = do
    testPositive 100 0
    testWrongForms


{-time spent: 30 min
Valid formulas seem to always get accepted. It's hard to test for more complex ones though, because
their evaluation and generation complexity grows exponentially.

However, invalid forms aren't always rejected. For example, `1 *(1 2)` is evaluated as 1, which shouldn't happen.
The parser should either throw an error, or empty formula for each invalid one, which apparently doesn't happen.

-}