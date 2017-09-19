--Testing properties strength- time: 40 mins
module Exercise3 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


forall :: [Int] -> (Int -> Bool) -> Bool
forall = flip all

stronger, weaker :: [Int] -> (Int -> Bool) -> (Int -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

compar :: [Int] -> (Int -> Bool) -> (Int -> Bool) -> String
compar xs p q = let pq = stronger xs p q 
                    qp = stronger xs q p 
                in 
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"


quicksortProbs :: [Int] -> [Int] -> [Int] 
quicksortProbs range [] = []  
quicksortProbs range (x:xs) = 
   quicksortProbs range [a | a <- xs, elem (compar range (properties !! a) (properties !! x)) ["stronger","equivalent"]]  
   ++ [x]
   ++ quicksortProbs range [a | a <- xs, compar range (properties !! a) (properties !! x) == "weaker" ] 


{-
   a) Implement all properties as Haskell functions of type Int -> Bool. Consider a small domain like [(−10)..10]
-}

-- (\ x -> even x && x > 3) even
prop0, prop1, prop2, prop3 :: Int -> Bool
prop0 n | n>3 && even n = True
        | otherwise = False

prop1 n | even n = True
         | otherwise = False
		
-- (\ x -> even x || x > 3) even
prop2 n | n>3 || even n = True
        | otherwise = False
-- we can use prob11 for the right one

--(\ x -> (even x && x > 3) || even x) even
prop3 n = even n || (even n && n>3)
-- we can use prob11 for the right one


range :: [Int]
range = [(-10)..10]

properties :: [(Int -> Bool)]
properties = [prop0, prop1, prop2, prop3] -- indexed: [0..3]

strengthList :: [Int]
strengthList = quicksortProbs range [0..3] -- result is [0,3,1,2]

{- GHCi: 
	*Exercise3> strengthList
	[0,3,1,2]
-}

main = do
	print "prop0, prop1, prop2, prop3"
	print strengthList

{- Report:
	To automate the test, I considered a range of [-10..10]. I implemented each of the properties described in the question.
	I took out redundant properties. I ended up with 4 different properties.
	
	I adjusted the comparison functions from lecture2.hs to be compatible with the definition of my properties.
	And at the end I implemented a list of properties and passed it to a customized quick-sort function. The sorting method is based on the strength of properties when applied to all values in the given range [-10..10].
	
	My random list of properties is as follows:
													[prop0, prop1, prop2, prop3] [0,3,1,2]
	
	*Exercise3> strengthList
	[0,3,1,2]
	
	The sorted list of properties was returned as follows:
													[prop0, prop3, prop1, prop2] [0,3,1,2]
	
	The list is sorted from stongest to weakest condition.
-}