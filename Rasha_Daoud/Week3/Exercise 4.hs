--  formula generator -time: 1 hour

module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- We can use the following functions from lecture 2, to generate list of integers between 1 and n
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

----------------------------------------------------------
{- Specification: formula generator
The formula generator can generate Atomic statements (Prop name). Which we will name as a formula with depth (level) 0.
We can use level = 0 as a base case for the recursion, when generating forms with bigger depth.

While generating forms with a certain depth is considered as trees. 
level > 0 means that we have to generate other types of forms rather than atomic forms.

We can use a random Int generator from lecture 2 (see previous section) to get the Int (name) for atomic forms.

We can also use the same Int generator to generate the number of formulas we're adding in each level of the complex formula (tree),
while reducing level each time to (level -1) to go one step down until we reach level 0,
in which the generator will add an atomic form (leaf). 

Furthermore, we can also generate the type of the form we're creating in each level, using the Int generator.
The generated Int will be between 1 and 6. Based on the generated number,
we choose between form options (Prop, Neg, Dsj, Cnj, Impl , Equiv).
-}

-- Basecase - Atomic formula
generateForm :: Int -> IO Form
generateForm 0 = do
                    n <- getRandomInt 10
                    return (Prop n)

-- Other types of formulas
generateForm level = do
                        n <- getRandomInt 6
                        m <- getRandomInt 10
                        case n of 
                            1 -> do return (Prop m)
                            2 -> do
                                    frm <- generateForm (level-1)
                                    return (Neg frm) 
                            3 -> do
                                    frms <- generateForms m (level-1)
                                    return (Dsj frms)
                            4 -> do
                                    frms <- generateForms m (level-1)
                                    return (Cnj frms)
                            5 -> do
                                    frm1 <- generateForm (level-1)
                                    frm2 <- generateForm (level-1)
                                    return (Impl frm1 frm2) 
                            6 -> do
                                    frm1 <- generateForm (level-1)
                                    frm2 <- generateForm (level-1)
                                    return (Equiv frm1 frm2) 

-- Generates n formulas with level l
generateForms :: Int -> Int -> IO [Form]
generateForms 0 _ = return []
generateForms n l =   do
                       form <- generateForm l
                       forms <- (generateForms (n-1) l)
                       return (form:forms)

{-----------------------------------------------Testing properties---------------------------------------------------}
-- 1) the formula is not implication or equivalence.
hasNoArrows :: Form -> Bool
hasNoArrows form | form == arrowfree form = True
                     | otherwise = False

-- 2) the formula has noly properties & negations
hasOnlyNeg :: Form -> Bool
hasOnlyNeg (Prop n)   = True
hasOnlyNeg (Neg frm)  = hasOnlyNeg frm
hasOnlyNeg _          = False

-- 3) the formula is at depth of 0 or 1 (or negation only)
NotDeeperThan1 :: Form -> Bool
NotDeeperThan1 (Prop _)      = True
NotDeeperThan1 (Neg frm)     = hasOnlyNeg frm
NotDeeperThan1 (Neg _)       = False
NotDeeperThan1 (Equiv _ _)   = False
NotDeeperThan1 (Impl _ _)    = False
NotDeeperThan1 (Cnj frms)    = all (hasOnlyNeg frms)
NotDeeperThan1 (Dsj frms)    = all (NotDeeperThan1 frms)


{- Report: 
   
-}