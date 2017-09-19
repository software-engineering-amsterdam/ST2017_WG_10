module Lab3 where

import Data.List
import Data.Char
import Data.String.Utils
import System.Random
import Test.QuickCheck
import Lecture3

-- 60 min
-- To test the parser I will feed it some valid and invalid strings
-- The precondition is that the input string should only contain valid characters, 
-- which is the precondition for the lexer
-- The postcondition is that the output list should contain one element
-- which is equal to the input string

-- Valid characters
validChars :: [Char]
validChars = ['(',')','*','+','-','=','<','>',' ','0','1','2','3','4','5','6','7','8','9']

validInput :: [Char] -> Bool
validInput s = s == [c|c <- s,elem c validChars]

tests :: [([Char],Bool)]
tests = [
          ("1", True),
          ("10984", True),
          ("abc", False),
          ("f", False),
          ("-", False),
          ("1-", False),
          ("-(1)", False),
          ("(-1)", False),
          ("-*(1 2)", True),
          ("-(*(1 2))", False),
          ("-1", True),
          ("--1", True),
          ("---1", True),
          ("*(1 2 3)", True),
          ("*1 2 3", False),
          ("(1 * 2 * 3)", False),
          ("+(1 2 3)", True),
          ("+1 2 3", False),
          ("(1 + 2 + 3)", False),
          ("*(1 +(2 3))", True),
          ("(1==>2)", True),
          ("(1<=>2)", True),
          ("*((1==>2) (2==>1))", True),
          ("+((1==>2) (2==>1))", True),
          ("(1<=>2)", True),
          ("(*((1==>2) (2==>1))==>(1<=>2))", True),
          ("(*(1 -1)<=>+(1 2))", True)
        ]

testParse :: ([Char],Bool) -> Bool
testParse t = if(validInput (fst t)) -- Will get through lexer?
                then -- Valid input
                  if(length (parse (fst t)) == 1) 
                    then  -- Valid input and a valid form. Was that expected?
                      ( show ( head ( parse (fst t) ) ) == fst t ) == snd t 
                    else -- Valid input but NOT a valid form. Was that expected?
                      snd t == False
                  else -- Invalid input, was that expected?
                    validInput (fst t) == snd t
                    
testParser :: [Char]
testParser  | all (\t -> testParse t) tests = "Passed all tests"
            | otherwise = "The following tests failed: [ " ++ join " ], [ " [fst x | x <- tests, not (testParse x)] ++ " ]"