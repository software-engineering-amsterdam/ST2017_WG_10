--  formula generator -time: 1 hour

module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


-- intermediate convertor from proposational form into CNF, we apply it after we remove arrows and then apply nnf - lecture 3 slides
cnf :: Form -> Form
cnf (Prop n)       = Prop n
cnf (Neg (Prop n)) = Neg (Prop n)
cnf (Neg frm)      = Neg (cnf frm)
cnf (Dsj frms)     = if (length frms) == 0 then (Dsj []) else if (length frms) == 1 then do (cnf (head frms))
                     else (applyMorganLaw (cnf (head frms)) (Dsj (tail frms)))
cnf (Cnj frms)     = Cnj (map cnf frms)
cnf _              = Cnj []

-- morgan law needed for the conversion
applyMorganLaw :: Form -> Form -> Form
applyMorganLaw frm frm'                     = Cnj [frm, frm']
applyMorganLaw (Cnj (frm:frms)) frm'        = Cnj [applyMorganLaw frm frm', applyMorganLaw (Cnj frms) frm']
applyMorganLaw frm (Cnj (frm':frms))        = Cnj [applyMorganLaw frm frm', applyMorganLaw frm (Cnj frms)]

-- converts a proposational form into CNF
convert2CNF :: Form -> Form
convert2CNF form = cnf $ nnf $ arrowfree form -- lecture 3 slides


-------------
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
{- Specifications: formula generator
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

-- 2) the converted CNF formula is NNF
isnnf :: Form -> Bool
isnnf form | form == nnf form = True
                 | otherwise = False

{- 3) the formula is indeed a CNF -}
-- 3.1) helper function
isdsj :: Form -> Bool
isdsj (Cnj frms)       = False
isdsj (Dsj (frm:frms)) = (isdsj (Dsj frms)) && (isdsj frm)
isdsj _                = True

-- 3.2) main function to check whether a statement is in cnf form
iscnf :: Form -> Bool
iscnf (Cnj frms)       = all iscnf frms
iscnf (Dsj (frm:frms)) = (iscnf (Dsj frms)) && (isdsj frm)
iscnf _                = True


-- This is the main property, to check whether a form is a CNF, using previous defined properties
testCNF :: Form ->  Bool
testCNF frm = (hasNoArrows frm) &&
               (isnnf frm)&&
               (iscnf frm)

{- Let us test the n generated forms with the defined property.
We have to convert each of the generated forms to CNF using the convertor from exercise 3, before we check CNF properties. -}
testIter :: Int -> (Form -> Bool) -> [Form] -> IO()
testIter n p [] = print (show n ++ " tests passed...")
testIter n p (fi:fis) = 
                        if p (convert2CNF fi) then
                         do
                          print ("test passed on:" ++ show fi)
                          testIter n p fis
                        else
                          error ("test failed on:" ++ show fi)

testForms :: Int -> Int -> (Form -> Bool)-> IO()
testForms n l p = do 
                      forms <- generateForms n l
                      testIter n p forms

-- We can now generate 100 forms (n) with a certain level, convert them all to CNF and check whether testCNF property holds for all!!
test100Form:: Int -> IO()
test100Form level = testForms 100 level testCNF

{- Report: 
   Using similar form generator to the one implemented in exercise 2,
   we can generate 100 forms with a certain level. 
   before we check the generated form against the defined property testCNF or any sub property (3),
   we call convert2CNF to convert the generated form to CNF.
   
   Next, we can do the check.
   
   test100Form can test 100 generated forms with different types (depending on level, level 0 means only atomic items).
   All forms should hold for testCNF, because we're converting them before checking the property.
   
   In case we called test100Form with different levels (e.g 3 levels), each time we get 100 forms.
   
   If test100Form passed successfully, that means we've succeeded to test the property against 300 forms.
   
-}