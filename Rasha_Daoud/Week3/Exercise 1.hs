-- lab1 -time: 20 minutes

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
entails form1 form2 =  (all(\x -> evl x form1 --> evl x form2) (allVals form1))
                       &&
                       (all(\x -> evl x form1 --> evl x form2) (allVals form2))
{- GHCi:
		*Exercise1> entails form1 form1
		True
		*Exercise1> entails (Impl p q) (Impl (Neg q) (Neg p))
		True
-}

----------------------------------------------------------------------------------------------------
{- two statements or formulas are equivalent, if no matter x is, evaluating form1 & form2 with x return the same truth value -}
equiv :: Form -> Form -> Bool
equiv form1 form2 =    (all(\x -> evl x form1 == evl x form2) (allVals form1))
                       &&
                       (all(\x -> evl x form1 == evl x form2) (allVals form2))

{- GHCi:
		*Exercise1> equiv (Impl p q) (Impl (Neg q) (Neg p))
		True
-}

----------------------------------------------------------------------------------------------------
{-Check that your definitions are correct.-}


