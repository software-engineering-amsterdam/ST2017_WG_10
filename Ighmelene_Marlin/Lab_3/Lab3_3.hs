module Lab3_3 where

import Data.List
import Data.Char
import Data.String.Utils
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3_1

cnf :: Form -> Form
cnf (Prop p)        = Prop p
cnf (Neg (Prop p))  = Neg (Prop p)
cnf (Neg f)         = cnf (Neg f)
cnf (Dsj fs)        = replacementLaws (Dsj fs)
cnf (Cnj fs)        = associativity (Cnj (map cnf fs))
cnf f               = (cnf.nnf.arrowfree) f

-- Apply the replacement laws to complete the transformation to CNF
-- 1) Removes disjunctions over conjunctions to distributivity
-- 2) Groups similar formulas through associativity
-- 3) Reorders children formulas through commutativity
-- 4) Removes duplicate formulas
replacementLaws :: Form -> Form
replacementLaws f = (removeDuplicates.commutativity.associativity.distributivity) f

-- Only applies distributivity over disjunctions to replace disjunctions with conjunctions
distributivity :: Form -> Form
distributivity (Dsj [p])      = distributivity p
distributivity (Dsj [p,q])    = distributivityDsj p q
distributivity (Dsj (p:q:fs)) = let redistributed = distributivityDsj p q in replacementLaws (Dsj ([redistributed]++fs))
distributivity f              = f

-- Only applies distributivity over disjunctions to replace disjunctions with conjunctions
distributivityDsj :: Form -> Form -> Form
distributivityDsj (Cnj ls)  (Cnj rs)  = Cnj [replacementLaws (Dsj [l,r])  | l <- ls, r <- rs]
distributivityDsj (Dsj ls)  (Dsj rs)  = replacementLaws (Dsj (ls ++ rs ))
distributivityDsj ls        (Cnj rs)  = Cnj [replacementLaws (Dsj [ls,r]) | r <- rs]
distributivityDsj (Cnj ls)  rs        = Cnj [replacementLaws (Dsj [l,rs]) | l <- ls]
distributivityDsj fl        fr        = Dsj [fl,fr]

-- Only applies association over conjunctions and disjunctions by grouping children forms of the same type
-- Assumption: joining to children forms of the same type does not change the meaning of the parent form
associativity :: Form -> Form
associativity (Dsj [p,q])    = associativityDsj p q
associativity (Cnj [p,q])    = associativityCnj p q
associativity (Dsj (p:q:fs)) = let redistributed = associativityDsj p q in associativity (Dsj ([redistributed]++fs))
associativity (Cnj (p:q:fs)) = let redistributed = associativityCnj p q in associativity (Cnj ([redistributed]++fs))
associativity f              = f

-- Only applies association over disjunctions
associativityDsj :: Form -> Form -> Form
associativityDsj (Dsj ls)       (Dsj rs)        = Dsj (ls ++ rs)
associativityDsj (Prop p)       (Dsj rs)        = Dsj ([Prop p] ++ rs)
associativityDsj (Dsj ls)       (Prop p)        = Dsj (ls ++ [Prop p])
associativityDsj (Neg (Prop p)) (Dsj rs)        = Dsj ([Neg (Prop p)] ++ rs)
associativityDsj (Dsj ls)       (Neg (Prop p))  = Dsj (ls ++ [Neg (Prop p)])
associativityDsj (Dsj ls)        rs             = Dsj (ls ++ [rs])
associativityDsj ls             (Dsj rs)        = Dsj ([ls] ++ rs)
associativityDsj fl              fr             = Dsj [fl,fr]

-- Only applies association over conjunctions
associativityCnj :: Form -> Form -> Form
associativityCnj (Cnj ls)        (Cnj rs)       = Cnj (ls ++ rs)
associativityCnj (Prop p)        (Cnj rs)       = Cnj ([Prop p] ++ rs)
associativityCnj (Cnj ls)        (Prop p)       = Cnj (ls ++ [Prop p])
associativityCnj (Neg (Prop p))  (Cnj rs)       = Cnj ([Neg (Prop p)] ++ rs)
associativityCnj (Cnj ls)        (Neg (Prop p)) = Cnj (ls ++ [Neg (Prop p)])
associativityCnj (Cnj ls)        rs             = Cnj (ls ++ [rs])
associativityCnj ls             (Cnj rs)        = Cnj ([ls] ++ rs)
associativityCnj fl              fr             = Cnj [fl,fr]

-- Only applies commutation over conjunctions and disjunctions by sorting the forms in the form.
commutativity :: Form -> Form
commutativity (Dsj fs)  = Dsj (sort fs)
commutativity (Cnj fs)  = Cnj (sort fs)
commutativity fs        = fs

-- Remove duplicated children froms from a parent form
removeDuplicates :: Form -> Form
removeDuplicates (Dsj fs) = Dsj (unique fs)
removeDuplicates (Cnj fs) = Cnj (unique fs)
removeDuplicates fs       = fs

-- Remove duplicate elements from a list
unique :: Eq a => [a] -> [a]
unique [] = []
unique xs = do
              let top     = init xs
              let bottom  = last xs
              if(elem bottom top) then 
                unique top 
              else 
                (unique top) ++ [bottom]