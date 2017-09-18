module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

frequencies :: Int -> Int -> IO [Float]
frequencies n parts = do
            xs <- probs n
            let step = 1.0/fromIntegral(parts)
            let freqs = [fromIntegral(length(x))/fromIntegral(n) |
                 x <- [filter(\k -> (k > i*step) && (k < (i+1)*step)) xs |
                     i <- map (fromIntegral) [0..(parts-1)] ] ]
            return freqs
diffStep :: [Float] -> [Float]
diffStep [] = []
diffStep (x:xs) = (map (\y -> abs(y-x)) xs)++(diffStep xs)

test :: Int -> Int -> IO()
test n k = do
            let tries = 100
            let parts = 4
            let tolerance = 0.05
            if k >= tries then print (show (tries) ++ " tests passed")
                else do
                    f <- frequencies n parts
                    if( ((maximum f) > 1.0 )|| ((minimum f) < 0.0)) then error ("Test failed. Generated number out of (0,1)") 
                        else do
                        let diffs = diffStep f
                        if ((maximum diffs) > tolerance) then
                            error ("Test failed. Histogram: "++ show f)
                        else do test n (k+1)
{-
test 10000 0
"100 tests passed"

test 5000 0
"100 tests passed"

test 1000 0
*** Exception: Test failed. Histogram: [0.228,0.242,0.251,0.279]

Report:
The larger the number of generated floats, the better the results. This is because in larger sample
there is less of a chance that one quartile will be overrepresented by chance. This is why this testing
is not the best one, the tolerance should be based on number of floats generated (the lower the number,
the more generous tolerance).

-}