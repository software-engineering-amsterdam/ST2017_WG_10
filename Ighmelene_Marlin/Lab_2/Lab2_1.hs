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

getQuartiles :: Int -> IO [Float]
getQuartiles n =  do
                  ys <- (probs n)
                  let q1  = fromIntegral (length ([x | x <- ys, x >  0,    x < 0.25]))
                  let q2  = fromIntegral (length ([x | x <- ys, x >= 0.25, x < 0.5]))
                  let q3  = fromIntegral (length ([x | x <- ys, x >= 0.5,  x < 0.75]))
                  let q4  = fromIntegral (length ([x | x <- ys, x >= 0.75, x < 1]))
                  let tot = fromIntegral n
                  let percentages = map ( \x ->  100 * x / tot) [q1,q2,q3,q4]
                  return (percentages)
