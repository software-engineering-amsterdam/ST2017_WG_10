module Lab3_5 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3_1
import Lab3_3

type Clause  = [Int]
type Clauses = [Clause]

propValue :: Form -> Int
propValue (Prop p)       = p
propValue (Neg (Prop p)) = negate p


clause :: Form -> Clause
clause (Prop p)       = [propValue (Prop p)]
clause (Neg (Prop p)) = [propValue (Neg (Prop p))]
clause (Dsj [])       = []
clause (Dsj (f:fs))   = (clause f) ++ (clause (Dsj fs))
clause f              = []


clauses :: Form -> Clauses
clauses (Cnj [])       = []
clauses (Cnj (f:fs))   = (cnf2cls f) ++ (cnf2cls (Cnj fs))


cnf2cls :: Form -> Clauses
cnf2cls (Prop p)        = [clause (Prop p)]
cnf2cls (Neg (Prop p))  = [clause (Neg (Prop p))]
cnf2cls (Dsj fs)        = [clause (Dsj fs)]
cnf2cls (Cnj [])        = []
cnf2cls (Cnj (f:fs))    = (cnf2cls f) ++ (cnf2cls (Cnj fs))
cnf2cls f               = error ("cnf2cls failed on: "++(show f))



intToProp :: Int -> Form
intToProp p | p < 0     = (Neg (Prop (abs p)))
            | otherwise = (Prop p)


dsjToForm :: Clause -> Form
dsjToForm dsj | length dsj == 1 = intToProp (head dsj)
              | otherwise       = Dsj (map intToProp dsj)

             
cls2cnf :: Clauses -> Form
cls2cnf cnj | length cnj == 1   = dsjToForm (head cnj)
            | otherwise         = Cnj (map dsjToForm cnj)
            
strToForm s = (head.parse) s

{-----------------------------------------------Testing properties---------------------------------------------------}

-- Check if the reversal of the conversion is logically equivalent to the original form

testReversal :: Form ->  Bool
testReversal frm = equiv frm ((cls2cnf.cnf2cls) frm)


reversal :: Form -> Form
reversal frm = (cls2cnf.cnf2cls) frm


getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n+1))


{- We can now generate 100 forms (n) with a certain level, convert them all to CNF and check whether our reversal property holds for all!!
  for that, let's take the formula generator implementation we did before.
  After we generate the form, we can apply cnf convertor to convert the form to CNF in 'testCnfIter' -}

generateForms :: Int -> Int -> IO [Form]
generateForms 0 _= return []
generateForms l c = do
            f <- generateForm c
            fs <- generateForms (l-1) c
            return (f:fs)

generateForm :: Int -> IO Form
generateForm c = do
        p <- getRandomInt 4
        if c == 0 then return (Prop p) else do
            x <- getRandomInt 5
            l <- getRandomInt 5
            f1 <- generateForm (c-1)
            f2 <- generateForm (c-1)
            xs <- generateForms l (c-1)
            case x of 
                1 -> do return (Prop p)
                2 -> do return (Neg f1)
                3 -> do return (Cnj xs)
                4 -> do return (Dsj xs)
                5 -> do return (Impl f1 f2)
                otherwise -> do return (Equiv f1 f2)

testCnfIter :: Int -> (Form -> Bool) -> [Form] -> IO()
testCnfIter n p [] = print (show n ++ " tests passed...")
testCnfIter n p (fi:fis) = 
                        if p (cnf fi) then
                         do
                          print ("test passed on:" ++ show (cnf fi))
                          testCnfIter n p fis
                        else
                          error ("test failed on:" ++ show (cnf fi))
  
testForms :: Int -> Int -> (Form -> Bool)-> IO()
testForms n l p = do 
                      forms <- generateForms n l
                      testCnfIter n p forms
  
test100Form:: Int -> (Form ->  Bool) -> IO()
test100Form level p = testForms 100 level p


{- Report: 

   Using similar form generator to the one implemented in Lab3_2,

   we can generate 100 forms with a certain level. 

   before we check the generated form against the defined property testCNF or any sub property (3),

   we call cnf convertor to convert the generated form to CNF.

  
   Next, we can do the check. test100Form can test 100 generated forms with different types (depending on level, level 0 means only atomic items).

   All forms should hold for testReversal, because we're converting them before checking the property.

   In case we called test100Form with different levels (e.g 3 levels), each time we get 100 forms.
   
   If test100Form passed successfully, that means we've succeeded to test the property against 300 forms.

   You can execute the following main, to check the report because it is too long to be added to the report.

-}


main = do
          print "Testing properties...."
          test100Form 0 testReversal
          test100Form 1 testReversal
          test100Form 2 testReversal

-- time: 1 hour