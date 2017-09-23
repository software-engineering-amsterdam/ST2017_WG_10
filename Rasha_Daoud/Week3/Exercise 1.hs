-- lab1 -time: 30 minutes

module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{- helper function to check whether applying function on any element of a list returns always false -}
noElemSatisfies :: (a -> Bool) -> [a] -> Bool
noElemSatisfies p [] = True -- neutral boolean value for base case of recursion
noElemSatisfies p (x:xs) = not((p x) || (noElemSatisfies p xs))
-- which returns true in case the application of p on all elements of x:xs returns false

{- a statement or formula is a contradiction if evaluating it with any x returns true -}
contradiction :: Form -> Bool
contradiction form = noElemSatisfies (\x -> evl x form) (allVals form)

{- GHCi:
        *Exercise1> contradiction form1
        False
        *Exercise1> contradiction form2
        False
        *Exercise1> contradiction form3
        False
        *Exercise1> contradiction (Cnj [p, Neg p])
        True
-}


----------------------------------------------------------------------------------------------------
{- helper function to check whether applying function on any element of a list returns always true -}
allElementsSatisfy :: (a -> Bool) -> [a] -> Bool
allElementsSatisfy p [] = True -- neutral boolean value for base case of recursion
allElementsSatisfy p (x:xs) = (p x) && (allElementsSatisfy p xs)
-- which returns true in case the application of p on all elements of x:xs returns true

{- a statement or formula is a tautology if evaluating it with any x returns true -}
tautology :: Form -> Bool
tautology form = allElementsSatisfy (\x -> evl x form) (allVals form)

{- GHCi:
        *Exercise1> tautology form1
        True
        *Exercise1> tautology form2
        False
        *Exercise1> tautology form3
        True
-}

----------------------------------------------------------------------------------------------------
{- form1 entails form2, if for every (all) x, evaluating form1 with x is true, then evaluating form2 with x is also true -}
entails :: Form -> Form -> Bool
entails form1 form2 =  (all(\x -> evl x form1 --> evl x form2) (allVals form1)) &&
                       (all(\x -> evl x form1 --> evl x form2) (allVals form2))
{- GHCi:
		*Exercise1> entails form1 form1
		True
		*Exercise1> entails form2 form1
		True
		*Exercise1> entails (Impl p q) (Impl (Neg q) (Neg p))
		True
-}

----------------------------------------------------------------------------------------------------
{- two statements or formulas are equivalent, if no matter x, evaluating form1 & form2 with x return the same truth value -}
equiv :: Form -> Form -> Bool
equiv form1 form2 =  (all(\x -> evl x form1 == evl x form2) (allVals form1)) &&
                     (all(\x -> evl x form1 == evl x form2) (allVals form2))

{- GHCi:
		*Exercise1> equiv (Impl p q) (Impl (Neg q) (Neg p))
		True
		*Exercise1> equiv (Cnj [p,q]) (Cnj [p,q])
        True
-}

----------------------------------------------------------------------------------------------------
{-Check that your definitions are correct.
	I read a bit about the properties of these propositional logic functions & the relations between each of them.
	
	Thanks to the haskell road book & http://sites.millersville.edu/bikenaga/math-proof/truth-tables/truth-tables.html
	
	There are couple of facts (properties), I can count on to test if my implementation for the functions above is correct.
	
	1) a logical statement is satisfiable if it is not a contradiction,
	which means at least evaluating the statement with at least one x gives false.
	
	2) a logical statement is tautology if it is not a contradiction (at least one evaluation result is false), but also satisfiable
	
	3) two logical statements are equivalent, if one entails the other.
	
	4) a logical statement is contradiction, if it is not satisfiable (one false evaluation result)
	 and it is not a tautology.
	 
	5) two logical statements are equivalent, if each of them entails the other.

-}

-- Let's define some properties as follows:
property1, property2, property3:: Form -> Bool

property1 f1 = satisfiable f1 == not (contradiction f1)

property2 f1 = contradiction f1 == not (satisfiable f1) && not (tautology f1)

property3 f1 = tautology f1 == satisfiable f1 && not(contradiction f1)



property4, property5, property6 :: Form -> Form -> Bool

property4 f1 f2 = equiv f1 f2 == (entails f1 f2) && (entails f2 f1)

property5 f1 f2 = entails f1 (Dsj [f1,f2])

property6 f1 f2 = equiv f1 f2 == (entails f1 f2 && entails f2 f1)

{- GHCi: 
        *Exercise1> property1 form1
        True
        *Exercise1> property2 (Cnj [p, Neg p])
        True
        *Exercise1> property3 form1
        True
        *Exercise1> property4 (Impl p q) (Impl (Neg q) (Neg p))
        True
        *Exercise1> property5 form1 form2
        True
        *Exercise1> property6 (Neg p) (Neg (Neg (Neg p)))
        True
-}

-- We can check all properties using the following function
checkCorrectness :: Form -> Form -> Bool
checkCorrectness f1 f2    | satisfiable f1 = property1 f1
                          | contradiction f1 = property2 f1
                          | tautology f1 = property3 f1
                          | equiv f1 f2 = property4 f1 f2
						  | entails f1 (Dsj [f1,f2]) = property5 f1 f2
						  | equiv f1 f2 = property6 f1 f2
                          | otherwise = False

{- GHCi:
        *Exercise1> checkCorrectness form1 form2
        True
        *Exercise1> checkCorrectness form1 form1
        True
        *Exercise1> checkCorrectness (Cnj [p, Neg p]) form1
        True
        *Exercise1> checkCorrectness (Dsj [p, q]) (Dsj [q, p])
        True
        *Exercise1> checkCorrectness (Impl p q) (Impl (Neg q) (Neg p))
        True
        *Exercise1> checkCorrectness form1 form2
        True
-}
