module Lab4_8 where



import Data.List

import System.Random

import Test.QuickCheck    

import Lab4_5

import Lab4_6

import Lab4_7



-- 30 min



----------------

-- Properties --

----------------



-- Check if the symClos of the trClos of R is equal to trClos of the symClos or R

symTrClosEqTrSymClos :: Rel Int -> Bool

symTrClosEqTrSymClos rs = do

                            let symTrClos = (symClos.trClos) rs

                            let trSymClos = (trClos.symClos) rs

                            symTrClos == trSymClos

-----------

-- Tests --

-----------



testIsSymTrClosEqTrSymClos :: IO ()

testIsSymTrClosEqTrSymClos = do

                              let n =   100

                              cases <-  genRelations n

                              propTester_4_7_1 n symTrClosEqTrSymClos cases



qcTestIsSymTrClosEqTrSymClos :: IO ()

qcTestIsSymTrClosEqTrSymClos = verboseCheck symTrClosEqTrSymClos



{-

If the symClos or the trClos is equal to the trClos of the symClos then I should not find any test cases that prove otherwise.

The tests continuously fail on the first try. So the symmetric closure of the transitive closure of R IS NOT the same as the transitive closure of the symmetric closure of R



---



1) CUSTOM GENERATOR



testIsSymTrClosEqTrSymClos

"Failed on: [(32,34),(59,92),(3,55),(5,47),(15,85),(83,33),(35,9),(63,98),(42,73)]"



rs = [(32,34),(59,92),(3,55),(5,47),(15,85),(83,33),(35,9),(63,98),(42,73)]



(symClos.trClos) rs

[(3,55),(5,47),(9,35),(15,85),(32,34),(33,83),(34,32),(35,9),(42,73),(47,5),(55,3),(59,92),(63,98),(73,42),(83,33),(85,15),(92,59),(98,63)]



(trClos.symClos) rs

[(3,3),(3,55),(5,5),(5,47),(9,9),(9,35),(15,15),(15,85),(32,32),(32,34),(33,33),(33,83),(34,32),(34,34),(35,9),(35,35),(42,42),(42,73),(47,5),(47,47),(55,3),(55,55),(59,59),(59,92),(63,63),(63,98),(73,42),(73,73),(83,33),(83,83),(85,15),(85,85),(92,59),(92,92),(98,63),(98,98)]



---



2) QUICKCHECK TEST



qcTestIsSymTrClosEqTrSymClos 

Passed:  

[]



Passed: 

[]



Passed:  

[]



Passed:  

[]



Failed:  

[(0,-2),(0,3)]

*** Failed! 



Passed:                       

[]



Failed:                                       

[(0,3)]



Passed:                                    

[]



Passed:                                       

[(0,0)]



Failed:                                       

[(0,2)]



Passed:                                     

[]



Passed:                                       

[(0,0)]



Failed:                                       

[(0,1)]



Passed:                                     

[]



Passed:                                       

[(0,0)]



Falsifiable (after 5 tests and 3 shrinks -}