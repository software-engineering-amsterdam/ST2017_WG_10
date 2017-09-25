module Lab3_1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- 45 min
contradiction :: Form -> Bool
contradiction f = not ( any (\ v -> evl v f) (allVals f) )

-- tautology :: Form -> Bool
tautology f =  all (\ v -> evl v f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = do 
                  let f1Trues = filter (\ v -> evl v f1) (allVals f1)
                  let f2Trues = filter (\ v -> evl v f2) (allVals f2)
                  f2Trues == [x | x <- f2Trues, elem x f1Trues]

 -- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = (all (\ v -> evl v f1 == evl v f2) (allVals f1)) && 
              (all (\ v -> evl v f1 == evl v f2) (allVals f2))