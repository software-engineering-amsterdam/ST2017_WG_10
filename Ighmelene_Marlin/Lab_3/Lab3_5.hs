module Lab3_5 where

import Data.List
import Data.Char
import Data.String.Utils
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3_1
import Lab3_3

-- 60 min
type Clause  = [Int]
type Clauses = [Clause]

propValue :: Form -> Int
propValue (Prop p)       = p
propValue (Neg (Prop p)) = (-p)

clause :: Form -> Clause
clause (Prop p)       = [propValue (Prop p)]
clause (Neg (Prop p)) = [propValue (Neg (Prop p))]
clause (Dsj fs)       = [propValue f | f <- fs]

clauses :: Form -> Clauses
clauses (Cnj fs)  = [clause f | f <- fs]

cnf2cls :: Form -> Clauses
cnf2cls (Prop p)        = [clause (Prop p)]
cnf2cls (Neg (Prop p))  = [clause (Neg (Prop p))]
cnf2cls (Dsj fs)        = [clause (Dsj fs)]
cnf2cls (Cnj fs)        = clauses (Cnj fs)

intToProp :: Int -> Form
intToProp p = Prop p

dsjToForm :: Clause -> Form
dsjToForm dsj | length dsj == 1 = intToProp (head dsj)
              | otherwise       = Dsj (map intToProp dsj)
              
cls2cnf :: Clauses -> Form
cls2cnf cnj | length cnj == 1   = dsjToForm (head cnj)
            | otherwise         = Cnj (map dsjToForm cnj)