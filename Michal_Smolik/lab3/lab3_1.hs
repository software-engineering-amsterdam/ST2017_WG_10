module Lab3_1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

--we are looking if every evaluation of literals results in formula being false
--more precisely, if there's not a single evaluation which would cause formula to be true
contradiction :: Form -> Bool
contradiction f = not $ any (\x -> evl x f) $ allVals f

--simply checking if all evaluations result in formula being true
tautology :: Form -> Bool
tautology f = all (\x -> evl x f) $ allVals f

--checking if for every evaluation, f --> g holds
entails :: Form -> Form -> Bool
entails f g = and $ zipWith (-->) (map (\x -> evl x f) $ allVals f) (map (\x -> evl x g) $ allVals g)

--self-explanatory
equivalence :: Form -> Form -> Bool
equivalence f g = (entails f g) && (entails g f)

--time spent: 5 min