-- testing parse 
module Lab3_2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{-
	The lecture notes of this week define a function parse for parsing propositional formulas. Test this function.
-}

-- We can use the following functions from lecture 2, changing 0 to 1
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


-----------------------------------------------------------------------------------------------------------------------
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

-- Other types of formulas (complexity or level > 0)
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


testIter :: Int -> (Form -> Bool) -> [Form] -> IO()
testIter n p [] = print (show n ++ " tests passed...")
testIter n p (fi:fis) = 
                        if p fi then
                         do
                          print ("test passed on:" ++ show fi)
                          testIter n p fis
                        else
                          error ("test failed on:" ++ show fi)
  
testForms :: Int -> Int -> (Form -> Bool)-> IO()
testForms n l p = do 
                      forms <- generateForms n l
                      testIter n p forms

-- testParse to call the test function 100 times with a certain level.
testParse :: Int -> IO()
testParse level = testForms 100 level (\str -> let [forms] = parse(show str) in show str == show forms)


{---------------------------------------Testing false positives----------------------------------------}
-- We can prepare a list of invalid forms
invalidForms = [
                 "(3 - <=> 4)", "*1 ==> 4)", "(+-*(3 2)", 
                 "))(*(3 )", "+( *(3 4)+(2 1)" , "-+2 3)"
                ]
-- if parse returned [] for invalid forms, then the test is valid. Parse should not crash!!
testParseInvalidForms :: IO Bool
testParseInvalidForms = return (all (\x -> (parse x) == []) invalidForms)


{---------------------------------------Testing parse----------------------------------------}
main = do
          print "Testing parse with atomic forms:"
          testParse 0;
          print "Testing parse with forms (depth 1):"
          testParse 1;
          print "Testing parse with forms (depth 2):"
          testParse 2;
          print "Testing parse with invalid forms:"
          testParseInvalidForms;
{- The test passed with three different depth values. Which means 300 tests.

   Furthermore parser was able to survive invalid formulars. Which means that the implementation is correct.
   
   
    Valid formulas seem to always get accepted. It's hard to test for more complex ones though, because
    their evaluation and generation complexity grows exponentially.

    However, invalid forms aren't always rejected. For example, "1 *(1 2)" is evaluated as 1, which shouldn't happen.
    The parser should either throw an error, or empty formula for each invalid one, which apparently doesn't happen.
	
                                      ------------------------------------------

Let's try with depth 1 & the pre-defined false positives:

"Testing parse with forms (depth 1):"
"test passed on:+(7 7 9 1 1 1 6 7 6)"
"test passed on:(3<=>5)"
"test passed on:+(8 2 3 8)"
"test passed on:10"
"test passed on:+(10 5 2 4 4 7 1 8)"
"test passed on:(7==>6)"
"test passed on:-10"
"test passed on:8"
"test passed on:-3"
"test passed on:(1<=>5)"
"test passed on:10"
"test passed on:(10==>6)"
"test passed on:5"
"test passed on:-1"
"test passed on:(5<=>8)"
"test passed on:6"
"test passed on:+(4 3)"
"test passed on:(9==>1)"
"test passed on:+(9 6 1 5 3 10 3 4 6 9)"
"test passed on:(9<=>1)"
"test passed on:(5==>7)"
"test passed on:+(6 4 5 3)"
"test passed on:(9<=>9)"
"test passed on:-1"
"test passed on:*(2 2)"
"test passed on:+(9 8 7 8 3 7 8 8)"
"test passed on:(8==>3)"
"test passed on:*(3 10 8 4)"
"test passed on:(4==>3)"
"test passed on:(7==>7)"
"test passed on:(4==>1)"
"test passed on:*(4 2 7 8 3)"
"test passed on:(9==>9)"
"test passed on:9"
"test passed on:10"
"test passed on:+(9)"
"test passed on:2"
"test passed on:*(9 8 2 2 6 5 10 10 8)"
"test passed on:-1"
"test passed on:*(9 5 10 10 1 7 6)"
"test passed on:*(10 7 8 5 10 7)"
"test passed on:(4==>8)"
"test passed on:-6"
"test passed on:(6==>4)"
"test passed on:-4"
"test passed on:(2<=>2)"
"test passed on:(6<=>4)"
"test passed on:(7<=>8)"
"test passed on:(10<=>2)"
"test passed on:*(8 8 7 9 10)"
"test passed on:+(7 6 7)"
"test passed on:+(10 2 4 9 6)"
"test passed on:+(10 4 2 5 10 10)"
"test passed on:3"
"test passed on:*(5 7)"
"test passed on:(8<=>2)"
"test passed on:+(2)"
"test passed on:+(6 5 9 7 8 5 5 3)"
"test passed on:+(10 8 4 8 9)"
"test passed on:*(4 4 10 9 10 7 2)"
"test passed on:*(9 10 9 1 6 4)"
"test passed on:+(3 2 2 7 10 5)"
"test passed on:+(4 2 4)"
"test passed on:(9==>2)"
"test passed on:5"
"test passed on:-1"
"test passed on:(5==>5)"
"test passed on:+(6 10)"
"test passed on:+(4 1 10 9 6 5 5)"
"test passed on:-2"
"test passed on:(9<=>3)"
"test passed on:(8==>10)"
"test passed on:-4"
"test passed on:(7<=>8)"
"test passed on:(2==>6)"
"test passed on:*(7 6 7 6)"
"test passed on:(3==>2)"
"test passed on:*(7 5 4 8 9 3 9 5)"
"test passed on:5"
"test passed on:-7"
"test passed on:+(8 2 7 7 2 9)"
"test passed on:-7"
"test passed on:7"
"test passed on:(3<=>10)"
"test passed on:(8==>8)"
"test passed on:(4==>1)"
"test passed on:*(5 8)"
"test passed on:(9==>2)"
"test passed on:+(3 8 4 10 4)"
"test passed on:(1<=>8)"
"test passed on:*(9 5 8 8 3 7 5)"
"test passed on:7"
"test passed on:+(10 10 8 3 4 3 5 4 10)"
"test passed on:(10<=>3)"
"test passed on:9"
"test passed on:-4"
"test passed on:2"
"test passed on:(1<=>2)"
"test passed on:(5==>6)"
"test passed on:1"
"100 tests passed..."

"Testing parse with invalid forms:"
True

-}


--time: 1 hour, 10 minutes