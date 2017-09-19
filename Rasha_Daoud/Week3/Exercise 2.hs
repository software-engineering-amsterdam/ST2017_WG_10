-- testing parse -time:

module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{-  satisfiable :: Form -> Bool
	satisfiable f = any (\ v -> evl v f) (allVals f)
-}

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
--Let us generate some forms, based on a level (0,1,2..etc.) we can define to which level in the expression we will go
-- 0 means Prop Name, if you go to 1 then it will start using other types
generateForm :: Int -> IO Form
generateForm level =if (level < 0) then error "level of complexity cannot be below 0"
					else if (level == 0) then
					do	
						n <- getRandomInt 6 -- we have 6 types of forms, we generate random number between 1 & 6
						m <- getRandomInt 10 -- to not have the atomic always as (Prop 1), you can change 10 to int >0
						case n of
							1 -> do return (Prop m)
							2 -> do return (Neg (Prop m))
							3 -> do return (Cnj [(Prop m), (Prop m-1)])
							4 -> do return (Dsj [(Prop m), (Prop m-1)])
							5 -> do return (Impl [(Prop m), (Prop m-1)])
							6 -> do return (Equiv (Dsj [(Prop m), (Prop m-1)])  Equiv (Dsj [(Prop m-1), (Prop m)]) )
							otherwise -> print("the generator is not working as expected")
			
generateForms :: Int -> Int -> IO [Form]
generateForms 0 _ = return []
generateForms n l = if (n == 0) then print ("done generating random forms")
					else do
						form <- generateForm l
						forms <- (generateForms (n-1) l)
						return (form:forms)

test1 :: Int -> (Form -> Bool) -> [Form] -> IO()
test1 n _ [] = print (show n ++ " tests passed...")
test1 n p (fi:fis) =  if p fi then
						do print ("test passed on:" ++ show fi)
							test1 n p fis
						else
							error ("test failed on:" ++ show fi)

testForm :: Int -> Int -> (Form -> Bool)-> IO()
testForm n l p = do 
					forms <- generateForms n l
					test1 n p forms

testParse = testForm 100 0 (\str -> let [forms] = parse (show str) in show str == show forms)