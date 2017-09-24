module Lab3 where
import Data.List (intercalate, group, sort)
import Lecture3
import Lab3_1
import Lab3_2
import Lab3_3


--generator is in lab2. Test looks if random formula's conversion to CNF is equivalent to the formula, and if it actually is in CNF.
test :: Int -> Int -> IO ()
test k n = do
    if k == n then print(show n ++ " tests passed") else do
        f <- generateFormula 2
        let c = cnf f
        if (not (equivalence c f)) then error ("Test failed. Formula " ++ show f ++ " is not equivalent to CNF "++ show c) else do
            if (not (isCNF c)) then error ("Test failed. " ++ show c ++ " is not CNF") else do
                test (k+1) n


---------------------------------------------------------------
-----------------------TESTED PROPERTIES-----------------------
---------------------------------------------------------------

--tests to see if the list of forms contains only literals or their negations
onlyLiterals :: [Form] -> Bool
onlyLiterals [] = True
onlyLiterals (Neg(Prop f):xs) = onlyLiterals xs
onlyLiterals ((Prop f):xs) = onlyLiterals xs
onlyLiterals _ = False

--test for seeing if only literals are negated in a formula
onlyInnerNegs :: Form -> Bool
onlyInnerNegs (Prop f) = True
onlyInnerNegs (Neg (Prop f)) = True
onlyInnerNegs (Dsj xs) = all onlyInnerNegs xs
onlyInnerNegs (Cnj xs) = all onlyInnerNegs xs
onlyInnerNegs _ = False

--self-explanatory
isArrowFree :: Form -> Bool
isArrowFree (Prop f) = True
isArrowFree (Neg f) = isArrowFree f
isArrowFree (Dsj xs) = all isArrowFree xs
isArrowFree (Cnj xs) = all isArrowFree xs
isArrowFree (Impl x y) = False
isArrowFree (Equiv x y) = False

--sees if form is a disjunction of literals and nothing more complex
isDsjOfLiterals :: Form -> Bool
isDsjOfLiterals (Prop f) = True
isDsjOfLiterals (Neg (Prop f)) = True
isDsjOfLiterals (Dsj xs) = onlyLiterals xs
isDsjOfLiterals _ = False

--combination of these properties. CNF can take form of single literal, one disjunction or conjunction of
--disjunctions and literals. We cover all of these possibilities case by case.
isCNF :: Form -> Bool
isCNF (Prop f) = True
isCNF (Neg (Prop f)) = True
isCNF (Dsj xs) = onlyLiterals (xs)
isCNF (Cnj xs) = isArrowFree (Cnj xs) && onlyInnerNegs (Cnj xs) && all isDsjOfLiterals xs

main = do test 0 1000

--time spent: 30 min
{-
We are testing simpler formulas of complexity maximum of 2, because the conversion to CNF is a time-consuming process.
Apparently our CNF implementation is correct, or there is a same mistake in CNF converter and test which cancel out.
-}