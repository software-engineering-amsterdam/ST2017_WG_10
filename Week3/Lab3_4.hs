module Lab3_4 where

import Data.List (intercalate, group, sort)
import Lecture3
import Lab3_1
import Lab3_2
import Lab3_3


--generator is in Lab3_2. Test looks if random formula's conversion to CNF is equivalent to the formula, and if it actually is in CNF.

test :: Int -> Int -> IO ()
test k n = do
    if k == n then print(show n ++ " tests passed") else do
        f <- generateForm 2
        let c = cnf f
        if (not (equiv c f)) then error ("Test failed. Formula " ++ show f ++ " is not equivalent to CNF "++ show c) else do
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


-- test whether the form has no arrows (no implication or equivalent)
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

main = do
         test 0 1000;
         test 1 1000;
         test 2 1000;
         test 3 1000;

--time : 40 minutes

{-

Report:
   Using similar form generator to the one implemented in Lab3_2,
   we can generate n forms  (e.g. 1000) with a certain level of complexity. 
   before we check the generated form against the defined property testCNF or any sub property (3),
   we call the cnf convertor to convert the generated form to CNF.
   
   Next, we can do the check.
   
   All forms should hold for isCNF, because we're converting them before checking the property.
   
   
  We are testing simpler formulas of complexity maximum of 3, because the conversion to CNF is a time-consuming process.

  Apparently our CNF implementation is correct, or there is a same mistake in CNF converter and test which cancel out.

   *Lab3_4> Lab3_4.main
       "1000 tests passed"
       "1000 tests passed"
       "1000 tests passed"
       "1000 tests passed"

-}