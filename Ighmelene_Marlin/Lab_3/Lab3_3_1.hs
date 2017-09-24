module Lab3_3_1 where

import Data.List
import Data.Char
import Data.String.Utils
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3_1

-- 120 min
lazy_cnf :: Form -> Form
lazy_cnf f  | tautology f     = let p = head (propNames f) in head (parse ("+("++show p++" -"++show p++")")) 
            | contradiction f = let p = head (propNames f) in head (parse ("*("++show p++" -"++show p++")")) 
            | otherwise       = do
                                let fv    = falseValuations ( nnf ( arrowfree f) )
                                let s     = valuationsToString fv
                                let form  = head (parse s)
                                if(show(form) == s) then
                                  if(equiv form f) then 
                                    nnf form 
                                  else 
                                    error "Conversion to CNF failed"
                                else
                                  error "Conversion to CNF failed"

-- Step 1: get the False valuations
falseValuations :: Form -> [Valuation]
falseValuations f = filter (\v -> not (evl v f)) (allVals f)

-- Step 2: get the formula string for each prop
propToString :: (Name,Bool) -> [Char]
propToString v = let (a,b) = v in if(b == True) then show a else "-"++show a

-- Step 3: get the formula string for each valuation
valuationToString :: Valuation -> [Char]
valuationToString ps = "*(" ++ (join " " (map (\p -> propToString p) ps)) ++ ")"

-- Step 4: get the formula of the negated valuations
valuationsToString :: [Valuation] -> [Char]
valuationsToString vs = "*(" ++ join " " (map (\v -> "-"++valuationToString v) vs)++ ")"
