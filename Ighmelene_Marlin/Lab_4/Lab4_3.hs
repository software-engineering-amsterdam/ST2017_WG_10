module Lab4_3 where

import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd
import Lab4_2

-- 60 min
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set a) b = list2set (filter (\x -> inSet x b) a)

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = list2set (a ++ b)

setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = do
                                  let a' = filter (\x -> not(elem x b)) a
                                  let b' = filter (\x -> not(elem x a)) b
                                  list2set (a' ++ b')

----------------
-- Properties --
----------------
-- Intersection: all elements e in Set I can be found in both sets A and B
propInBothSets :: Set Int -> Set Int -> Bool
propInBothSets a b = let (Set i) = setIntersection a b in all (\e -> (inSet e a) && (inSet e b)) i

-- Union: all elements e in Set U can be found in either set A or B
propInEitherSet :: Set Int -> Set Int -> Bool
propInEitherSet a b = let (Set u) = setUnion a b in all (\e -> (inSet e a) || (inSet e b)) u

-- Difference: all elements e in Set D can only be found in one of the sets A or B, but not in both
propInOnlyOneSet :: Set Int -> Set Int -> Bool
propInOnlyOneSet a b = let (Set d) = setDifference a b in all (\e -> not ((inSet e a) && (inSet e b))) d

-----------
-- Tests --
-----------
testIntersection :: IO ()
testIntersection = do
                    let n = 100
                    casesA <- genSets n
                    casesB <- genSets n
                    let cases = zip casesA casesB
                    propTester_4_3_2 n propInBothSets cases


-- testIntersection
-- ...
-- "Passed on: ({-8,-1,10}, {-2,0,1,2})"
-- "Passed on: ({-7,-6,-3,0,1,3,12,15,16,18}, {-10,-6,0,2,5,14,15})"
-- "Passed on: ({-6,0,2,4,8,14}, {-4})"
-- "Passed on: ({-12,-8,-7,-4,-1,1,6,10}, {-12,-4,-3,-2})"
-- "Passed on: ({-4,0,1,3,8}, {12,13,14})"
-- "Passed all tests"

testUnion :: IO ()
testUnion = do
              let n = 100
              casesA <- genSets n
              casesB <- genSets n
              let cases = zip casesA casesB
              propTester_4_3_2 n propInEitherSet cases

-- testUnion
-- ...
-- "Passed on: ({-11,-9,-8,-2,-1,0,3,12}, {-9,-5,-2,2,3,6,8,9})"
-- "Passed on: ({-7,-6,-4,0,3,6,8,9}, {-18,-13,-5,-3,0,7,12,16,18,19})"
-- "Passed on: ({-8,0,2}, {-13,-11,-8,0,3,4,5,14})"
-- "Passed on: ({-1}, {0})"
-- "Passed on: ({-5,-1,0,1,4,5}, {-4,-3,-2,0,2,4,5})"
-- "Passed all tests"

testDifference :: IO ()
testDifference = do
                  let n = 100
                  casesA <- genSets n
                  casesB <- genSets n
                  let cases = zip casesA casesB
                  propTester_4_3_2 n propInEitherSet cases

-- testDifference
-- ...
-- "Passed on: ({-4,0,4,6,12}, {-6,-5})"
-- "Passed on: ({2}, {-13,-12,-8})"
-- "Passed on: ({-1}, {-13,11})"
-- "Passed on: ({}, {-2,0})"
-- "Passed on: ({0,4}, {-2,0})"
-- "Passed all tests"

-- Tester
propTester_4_3_2 :: Int -> (Set Int -> Set Int -> Bool) -> [(Set Int,Set Int)] -> IO ()
propTester_4_3_2 n prop []        = print ("Passed all tests")
propTester_4_3_2 n prop (t:cases) = do
                                  let (a,b) = t
                                  if(prop a b) then do
                                    print ("Passed on: ("++(show a)++", "++(show b)++")")
                                    propTester_4_3_2 (n-1) prop cases
                                  else
                                    print ("Failed on: ("++(show a)++", "++(show b)++")")


----------------
-- QuickCheck --
----------------
qcTestIntersection :: IO ()
qcTestIntersection = verboseCheck propInEitherSet
-- qcTestIntersection
-- ...
-- Passed:
-- {-87,-82,-79,-78,-76,-72,-70,-67,-64,-62,-61,-60,-55,-53,-52,-40,-34,-33,-18,-16,-10,-8,-3,-2,0,21,30,32,37,40,47,64,71,73,93}
-- {-93,-92,-82,-81,-68,-62,-58,-50,-44,-37,-33,-2,1,5,9,16,17,23,24,31,40,41,43,53,56,61,73,86,90,94}
--
-- Passed:
-- {-95,-36,-35,-31,-9,7,12,19,25,31,36,41,44,49}
-- {-91,-86,-85,-81,-64,-61,-59,-56,-50,-48,-43,-42,-35,-28,-18,-1,3,6,10,15,17,21,25,35,42,52,53,54,78,79,80,92,94}
--
-- Passed:
-- {-90,-87,-82,-76,-67,-66,-64,-62,-60,-59,-56,-42,-41,-27,-25,-24,-13,-7,-6,-1,1,4,9,13,15,17,25,29,35,38,39,41,42,59,60,62,71,76,78,81,85,94}
-- {-96,-95,-88,-86,-85,-84,-81,-78,-77,-75,-73,-72,-69,-64,-62,-60,-56,-46,-43,-39,-32,-29,-27,-25,-12,-11,-7,-6,-5,-1,0,3,5,8,11,12,14,20,22,28,30,32,35,37,39,41,42,43,45,49,54,57,58,59,61,65,69,70,72,74,75,81,83,84,88,89,97}
--
-- Passed:
-- {-97,-94,-93,-89,-88,-81,-75,-73,-72,-68,-66,-50,-45,-43,-42,-40,-30,-27,-22,-21,-14,-13,-12,-11,2,5,12,13,14,18,20,22,23,29,33,35,37,43,44,45,46,48,53,55,58,65,66,69,70,79,80,83,91}
-- {-97,-94,-88,-81,-79,-78,-75,-71,-69,-66,-64,-62,-58,-56,-53,-50,-49,-48,-41,-35,-34,-33,-31,-25,-24,-23,-22,-18,-17,-14,-11,-9,-6,-2,1,4,5,6,11,13,17,20,25,28,35,41,43,45,53,54,62,63,64,65,74,75,76,78,81,83,84,85,86,91,95}
--
-- Passed:
-- {-95,-93,-86,-84,-75,-60,-53,-48,-45,-44,-37,-36,-35,-29,-17,-14,-13,-12,-10,-2,0,3,4,7,17,25,29,36,37,39,44,55,66,71,92,93,96}
-- {-95,-93,-90,-88,-87,-84,-74,-72,-65,-63,-62,-60,-59,-57,-54,-50,-47,-43,-41,-40,-39,-34,-33,-30,-28,-26,-24,-17,-16,-15,-9,-6,0,4,10,11,12,21,23,25,27,28,31,32,34,35,37,38,41,45,46,48,51,52,55,56,57,58,59,66,67,68,70,79,80,81,84,92,94,97}
--
-- +++ OK, passed 100 tests.

qcTestUnion :: IO ()
qcTestUnion = verboseCheck propInBothSets
-- qcTestUnion
-- ...
-- Passed:
-- {-95,-94,-92,-89,-88,-87,-85,-84,-82,-79,-77,-73,-72,-70,-68,-60,-53,-48,-44,-39,-35,-34,-33,-32,-28,-26,-23,-22,-21,-20,-19,-17,-16,-15,-6,-2,0,3,9,11,13,15,18,19,21,24,28,32,33,38,42,44,49,50,55,56,57,61,62,65,75,77,79,84,88,89,90,93}
-- {-92,-86,-82,-79,-77,-72,-70,-69,-66,-63,-61,-55,-54,-53,-52,-51,-41,-39,-34,-28,-27,-24,-21,-18,-15,-13,-9,-3,1,4,6,7,10,13,14,16,17,20,27,32,47,48,70,71,76,81,84,86,90,94}
--
-- Passed:
-- {-93,-89,-88,-82,-81,-80,-68,-65,-64,-63,-62,-61,-58,-56,-45,-41,-40,-38,-30,-29,-28,-23,-20,-17,-3,1,2,7,8,22,27,31,33,36,40,53,55,58,59,63,76,77,81,89}
-- {-93,-91,-90,-87,-83,-80,-78,-75,-74,-64,-60,-54,-43,-39,-36,-33,-25,-19,-17,-4,12,13,15,18,20,21,26,38,42,44,51,53,55,62,65,73,74,84,94}
--
-- Passed:
-- {-97,-89,-86,-82,-79,-77,-76,-68,-66,-64,-62,-60,-59,-58,-55,-53,-52,-51,-50,-39,-34,-21,-20,-19,-18,-15,-14,-13,-10,-6,-4,-3,0,1,3,7,8,9,15,18,21,22,23,24,27,32,35,44,48,53,55,56,62,64,67,69,71,73,76,79,84,85,86,87,88,90,93,94,95,97}
-- {-97,-92,-90,-71,-61,-60,-59,-54,-47,-44,-34,-28,-27,-23,-20,-14,-13,-8,-5,0,2,4,6,7,8,9,10,11,15,20,24,26,28,37,39,41,44,48,49,63,66,71,78,82,83,85,86,89,91,93,95}
--
-- Passed:
-- {-93,-92,-90,-76,-74,-67,-62,-61,-52,-45,-40,-39,-36,-27,-23,-22,-3,7,9,12,13,14,26,27,31,39,62,68,70,75,83,87,90,91,93,94}
-- {-83,-74,-67,-65,-61,-55,-49,-40,-18,-10,0,1,17,19,30,31,32,36,47,48,66,70,87,96}
--
-- Passed:
-- {-96,-94,-92,-82,-73,-61,-58,-52,-47,-35,-33,-32,-19,-17,-14,-6,-5,18,36,46,60,81}
-- {-99,-95,-87,-79,-74,-67,-66,-62,-58,-52,-42,-33,-32,-27,-22,-20,-10,4,15,23,41,59,61,64,85,87,89}
--
-- +++ OK, passed 100 tests.

qcTestDifference :: IO ()
qcTestDifference = verboseCheck propInOnlyOneSet
-- qcTestDifference
-- ...
-- Passed:
-- {-95,-92,-91,-90,-89,-84,-83,-82,-80,-78,-77,-73,-68,-61,-57,-55,-52,-43,-38,-35,-28,-20,-17,-16,20,21,22,25,30,31,42,43,51,52,58,60,63,69,74,77,82,89,93,94}
-- {-65,-44,-42,-38,-35,-34,-30,27,35,45,57,66,91}
--
-- Passed:
-- {-92,-91,-88,-85,-84,-83,-81,-79,-78,-75,-73,-71,-69,-66,-64,-61,-60,-55,-52,-38,-37,-32,-28,-27,-25,-24,-23,-20,-19,-16,-5,-3,2,4,9,10,12,13,18,19,21,23,28,34,36,40,43,49,51,52,65,66,68,73,74,76,77,79,82,84,87,88,93}
-- {-95,-92,-84,-74,-49,-44,-42,-35,-22,-21,-11,-7,-6,-1,23,31,42,44,47,58,62,72,80,82,93}
--
-- Passed:
-- {-97,-96,-95,-93,-92,-88,-87,-84,-79,-78,-74,-73,-72,-71,-65,-62,-61,-60,-59,-55,-53,-51,-49,-47,-46,-44,-42,-39,-37,-29,-26,-23,-22,-19,-16,-15,-13,-10,-7,-2,1,5,8,10,14,19,20,22,24,25,28,32,35,40,42,47,51,52,54,55,56,60,62,63,65,66,71,73,76,79,84,87,89}
-- {-95,-94,-90,-86,-85,-84,-82,-80,-73,-72,-70,-68,-65,-62,-56,-47,-42,-38,-37,-30,-23,-21,-15,-11,-7,-5,2,4,8,10,37,41,44,51,52,53,54,59,61,63,64,65,66,69,77,78,79,84,93,94,97}
--
-- Passed:
-- {-96,-91,-74,-70,-52,-50,-38,-35,-33,-24,-20,-16,-15,-9,1,2,6,11,13,14,16,29,47,49,51,63,66,67,69,74,76,77,92}
-- {-92,-74,-68,-49,-47,-29,-17,-16,-15,9,25,34,35,36,37,42,51,53,63,74,75,86,87,92,94}
--
-- Passed:
-- {-70,-69,-33,-32,-27,-21,13,61,62,71,73,87}
-- {-96,-87,-79,-75,-74,-66,-65,-64,-52,-45,-38,-33,-23,-19,-17,3,4,5,7,13,21,25,28,29,46,51,59,71,85,92,93}
--
-- +++ OK, passed 100 tests.