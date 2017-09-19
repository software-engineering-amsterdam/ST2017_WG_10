--  formula generator -time:

module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

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

----------------------------------------------------------
--Let us generate some forms, based on a depth (0,1,2..etc.) we can define to which level in the expression we will go
-- 0 means Prop Name, if you go to 1 then it will start using other types
generateForm :: Int -> IO Form
generateForm 0 = do
                    n <- getRandomInt 10
                    return (Prop n)

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

testParse :: Int -> IO()
testParse level = testForms 100 level (\str -> let [forms] = parse(show str) in show str == show forms)


main = do
          testParse 0
          testParse 1
          testParse 2