-- testing parse -time:

module Exercise2 where

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
{- Let us generate some forms, based on a depth (0,1,2..etc.) we can define to which level in the expression we will go
 -  0 means Prop Name (Atomic form)
 - we can also generate forms with a certain depth or level
 - considering forms as trees, level > 0 means we will start using other types of forms rather than the atomic.
-}

-- Atomic formula
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