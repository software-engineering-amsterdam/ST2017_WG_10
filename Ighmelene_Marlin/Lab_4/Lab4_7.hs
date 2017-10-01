module Lab4_7 where

import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd
import Lab4_2
import Lab4_5
import Lab4_6

-- 90 min

-- Quetion: Can you use QuickCheck?
-- Answer: Yes and without writing a custom generator. Since types just synonyms, QuickCheck knows how to generate them.

-- Taken form lectur 2
infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

----------------
-- Properties --
----------------

-- For every element (a,b) there should be an element (b,a)
isSymmetric :: Rel Int -> Bool
isSymmetric rs = all (\r -> flipRelExists r rs) rs

-- All relations (x,z) already exist
isTransitive :: Rel Int -> Bool
isTransitive rs = all (\r -> elem r rs) (rs @@ rs)

-- Closure should contain all elements from the original relation
containsRel :: Rel Int -> Rel Int -> Bool
containsRel rs cl = all (\r -> elem r cl) rs

-- Removing one pair will break the closure
isMinimumSC :: Rel Int -> Bool
isMinimumSC rs = all (\r -> let (a,b) = r in ((a /= b) --> (not (isSymmetric (delete r rs))))) rs

-- Removing one element that is not in the original relation will break the closure
isMinimumTC :: Rel Int -> Rel Int -> Bool
isMinimumTC cl rs =  (all (\r -> (not (elem r rs)) --> (not (isTransitive (delete r cl)))) cl)

isSymClos :: Rel Int -> Rel Int -> Bool
isSymClos cl rs = (isSymmetric cl) && (containsRel rs cl) && (isMinimumSC cl)

isTrClos :: Rel Int -> Rel Int -> Bool
isTrClos cl rs = (isTransitive cl) && (containsRel rs cl) && (isMinimumTC cl rs)

-----------
-- Tests --
-----------

-----------------------------
-- Symmetric Closure Tests --
-----------------------------

-- Check if symClos is symmetric
testIsSymmetric :: IO ()
testIsSymmetric = do
                    let n = 100
                    rels <- genRels n
                    let cases = map symClos rels
                    propTester_4_7_1 n isSymmetric cases
{-
testIsSymmetric
...
"Passed on: [(3,65),(3,68),(6,38),(12,25),(13,74),(25,12),(38,6),(42,52),(42,54),(44,57),(52,42),(54,42),(57,44),(63,99),(65,3),(68,3),(74,13),(75,84),(84,75),(99,63)]"
"Passed all tests"
-}

qcTestIsSymmetric :: IO ()
qcTestIsSymmetric = verboseCheck (\c -> isSymmetric (symClos c))
{-
qcTestIsSymmetric
...
Passed:   
[(-29,61),(-40,-81),(-93,-94),(-42,70),(-46,73),(80,92),(-83,-94),(60,-68),(-44,-29),(36,-18),(2,-96),(-80,52),(-95,65),(-43,-75),(49,-30),(-51,-1),(-49,32),(67,-3),(-14,-37),(-64,-38),(-35,-69),(33,-59),(-24,-67),(-33,95),(21,77),(66,66),(42,78),(-34,45),(-17,-22),(-32,-64),(5,43),(76,-71),(-78,38),(-92,32),(71,25),(-20,97),(-98,79),(28,95),(-17,3),(97,62),(83,52),(-84,99),(-1,33),(93,71),(-5,14),(5,-32),(81,82),(45,12),(18,34),(98,86),(47,-11),(-23,-61),(-67,45),(10,32),(72,94),(-57,-71),(27,10),(67,-59),(-58,-39),(-58,-6),(-34,60),(15,-31),(-56,46),(23,-64),(-92,-8),(34,-8),(-26,-39),(-38,1),(0,-26),(22,-42),(-58,-52),(89,57),(-1,-58),(-12,-38),(-84,-98),(-40,33),(87,-29),(64,80),(51,-86),(-95,50),(-77,-32),(-59,-25),(-40,-29),(19,38),(-29,-98),(0,20),(-89,85),(-43,-77),(-64,-59),(-27,81),(-59,-55),(37,-48),(-91,68),(-86,-28),(-25,-97),(97,77),(52,-32)]

+++ OK, passed 100 tests.
-}
                    
-- Check if this is the min relation (including original) to make the relation a symmetric closure
testIsMinimumSC :: IO ()
testIsMinimumSC = do
                    let n = 100
                    rels <- genRels n
                    let cases = map symClos rels
                    propTester_4_7_1 n isMinimumSC cases
{-
testIsMinimumSC
...
"Passed on: [(6,15),(15,6),(19,57),(24,28),(26,47),(28,24),(43,48),(47,26),(48,43),(57,19),(61,79),(79,61)]"
"Passed all tests"
-}

qcTestIsMinimumSC :: IO ()
qcTestIsMinimumSC = verboseCheck (\c -> isSymmetric (symClos c))
{-
qcTestIsMinimumSC
...
Passed:   
[(-47,-68),(16,98),(-43,84),(-69,-31),(94,42),(-17,-36),(53,-95),(-77,78),(-55,-17),(1,65),(-95,-31),(-45,40),(79,40),(55,15),(-50,60),(71,57),(-52,-5),(50,17),(-76,29),(-45,42),(-21,38),(13,60),(99,-19),(75,-97),(80,-38),(-13,17),(-57,-74),(-32,39),(-84,-61),(-14,63),(70,38),(34,-18),(-35,42),(30,-2),(14,-98),(61,-35),(28,-63),(5,84),(-48,-42),(-71,26),(97,-46),(-40,-95),(12,-9),(-32,92)]

+++ OK, passed 100 tests.
-}

-- Check if the closure includes the original relation
testContainsRelSc :: IO ()
testContainsRelSc = do
                      let n     =   100
                      rels      <-  genRels n
                      let cls   =   map symClos rels
                      let cases =   zip rels cls
                      propTester_4_7_2 n containsRel cases
{-
testContainsRelSc
...
"Passed on: [(93,73),(87,88),(94,38),(45,10),(43,55),(12,30),(97,48),(90,48)]"
"Passed all tests"
-}

qcTestContainsRelSc :: IO ()
qcTestContainsRelSc = verboseCheck (\c -> containsRel c (symClos c))
{-
qcTestContainsRelSc
...
Passed:   
[(-69,-4),(-18,29),(26,32),(-53,-67),(-77,10),(98,-8),(29,-61),(29,-12),(-48,99),(-43,73),(-18,-95),(88,-62),(-79,-87),(-60,-55),(77,8),(-8,61),(-27,95),(-42,67),(98,-39),(70,-9),(49,5),(-22,49),(67,85),(-84,39),(-34,25),(-10,-39),(-10,44),(-23,15),(39,67),(-31,33),(-30,-19),(-66,59),(82,-16),(8,9),(2,-95),(8,34),(-84,30),(-16,6),(-97,-47),(-48,64),(45,-84),(-36,-48),(-46,38)]

+++ OK, passed 100 tests.
-}

-- Check if the relations are symmetric closures
testIsSymClos :: IO ()
testIsSymClos = do
                  let n     =   100
                  rels      <-  genRels n
                  let cls   =   map symClos rels
                  let cases =   zip cls rels
                  propTester_4_7_2 n isSymClos cases
{-
testIsSymClos
...
"Passed on: [(4,17),(17,4),(23,72),(45,99),(53,75),(60,77),(72,23),(75,53),(77,60),(99,45)]"
"Passed all tests"
-}

qcTestIsSymClos :: IO ()
qcTestIsSymClos = verboseCheck (\c -> let cl = symClos c in isSymmetric cl ==> containsRel c cl ==> isMinimumSC cl)
{-
qcTestIsSymClos
...
Passed:   
[(16,32),(59,-76),(66,-15),(19,-52),(-74,20),(-29,82),(33,-48),(-57,-43),(-41,54),(-55,71),(-51,-72),(-9,-80),(-5,42),(67,-75),(-7,36),(24,-87),(-7,66),(-41,75),(55,-34),(98,-96),(51,93),(85,-95),(-25,-23),(-81,61),(-46,-35),(-69,63),(15,-92),(78,-37),(-73,-54),(-5,73),(19,-46),(-69,-41),(48,17),(79,-74),(-10,-24),(47,-32),(-10,32),(91,4),(45,-11),(-26,-23),(16,7),(-2,52),(75,-61),(-22,91),(-14,92),(-40,-42),(-87,-16),(94,-47),(63,18),(-62,83),(51,94),(-60,65),(-64,34)]

+++ OK, passed 100 tests.
-}

------------------------------
-- Transitive Closure Tests --
------------------------------

-- Check if trClos is transitive
testIsTransitive :: IO ()
testIsTransitive = do
                    let n = 100
                    rels <- genRels n
                    let cases = map trClos rels
                    propTester_4_7_1 n isTransitive cases
{-
testIsTransitive
...
"Passed on: [(6,78),(37,18),(63,100)]"
"Passed all tests"
-}

qcTestIsTransitive :: IO ()
qcTestIsTransitive = verboseCheck (\c -> isTransitive (trClos c))
{-
qcTestIsTransitive
...
Passed:   
[(-15,-3),(-36,-86),(-32,40),(24,40),(-38,31),(23,-71),(-66,-24),(-89,42),(-96,35),(13,58),(2,-21),(59,18),(39,-68),(-83,-28),(30,-53),(-95,-13),(39,3),(71,-84),(-64,29),(49,-36),(90,88),(-57,-99),(-50,-38),(43,83),(99,76),(-21,75),(-72,-14),(21,59),(-97,18),(59,-75),(32,-39),(-69,32),(2,19),(99,47),(-58,-82),(80,23),(26,-72)]

+++ OK, passed 100 tests.
-}

-- Check if this is the min relation (including original) to make the relation a transitive closure
testIsMinimumTc :: IO ()
testIsMinimumTc = do
                    let n     =   100
                    rels      <-  genRels n
                    let cls   =   map trClos rels
                    let cases =   zip cls rels
                    propTester_4_7_2 n isMinimumTC cases
{-
testIsMinimumTc
...
"Passed on: [(0,71),(0,99),(17,35),(20,43),(26,30),(38,74),(57,42),(86,80),(90,83)]"
"Passed all tests"
-}
                  
qcTestIsMinimumTc :: IO ()
qcTestIsMinimumTc = verboseCheck (\c -> isMinimumTC (trClos c) c)
{-
qcTestIsMinimumTc
...
Passed:   
[(-61,82),(-2,-8),(-10,-30),(58,66),(75,-78),(-51,-38),(-16,-59),(-95,67),(2,92),(-48,6),(-62,-16),(34,39),(-44,-76),(7,40),(29,70),(13,67),(50,62),(4,29),(0,-9),(45,34),(63,-6),(-75,83),(-49,-32),(-45,53),(75,77),(66,61),(-87,-46),(-69,-13),(8,92)]

+++ OK, passed 100 tests.
-}

-- Check if the closure includes the original relation
testContainsRelTc :: IO ()
testContainsRelTc = do
                      let n     =   100
                      rels      <-  genRels n
                      let cls   =   map trClos rels
                      let cases =   zip rels cls
                      propTester_4_7_2 n containsRel cases
{-
testContainsRelTc
...
"Passed on: [(8,62),(27,37),(1,96),(30,3),(34,42)]"
"Passed all tests"
-}

qcTestContainsRelTc :: IO ()
qcTestContainsRelTc = verboseCheck (\c -> containsRel c (trClos c))
{-
qcTestContainsRelTc
...
Passed:   
[(-66,-11),(20,79),(66,26),(-73,40),(-66,-9),(-69,-14),(81,-94),(12,51),(57,-9),(-71,-48),(98,-90),(27,83),(-90,21),(17,-56),(-21,-31),(-38,-44),(31,12),(-1,84),(-28,69),(-62,-23),(48,-77),(26,61),(33,-33),(-25,-13),(78,-73),(6,-1),(51,-23),(-73,76),(-3,-7),(-84,-40),(-84,-27),(-94,36),(27,20),(58,3),(71,68),(-5,-98),(-51,-21),(-56,28),(1,41),(-52,79),(-77,71),(-16,49),(43,-99),(-68,99),(-50,-73),(-95,-70),(-77,-48),(-37,-31),(70,60),(-75,-12),(-47,-61),(-49,17),(58,60),(-62,93),(71,20),(54,-29),(96,99),(-89,28),(-31,-53),(-78,-78),(15,-80),(34,-34),(28,11),(-61,-43),(53,-73),(71,-68),(-5,86),(-42,-10),(-30,-51),(93,64),(-61,-24),(-18,-65),(-84,98),(90,-12),(-34,93),(-77,34),(84,21),(-81,30),(-73,78),(-8,22),(-69,-68),(-42,7),(-96,-53),(0,-9),(93,65),(32,8)]

+++ OK, passed 100 tests.
-}

-- Check if the relations are transitive closures
testIsTrClos :: IO ()
testIsTrClos = do
                let n     =   100
                rels      <-  genRels n
                let cls   =   map trClos rels
                let cases =   zip cls rels
                propTester_4_7_2 n isTrClos cases
{-
testIsTrClos
...
"Passed on: [(1,95),(2,35),(19,56),(25,24),(34,58),(40,39),(69,39),(69,40),(79,28)]"
"Passed all tests"
-}

qcTestIsTrClos :: IO ()
qcTestIsTrClos = verboseCheck (\c -> let cl = trClos c in isTransitive cl ==> containsRel c cl ==> isMinimumTC cl c)
{-
qcTestIsTrClos
...
Passed:   
[(1,-18),(16,-21),(91,7),(32,-94),(-68,-84),(-57,25),(41,-25),(-98,-79),(28,-99),(68,-2),(-69,65),(-14,47),(54,17),(-64,-49),(31,8),(-42,-7),(77,-66),(65,-81),(75,21),(63,-56),(13,27),(-83,19),(-46,24),(-18,-28),(-2,-34),(84,17),(-83,-14),(-83,6),(56,-71),(27,9),(18,13),(-53,-60),(77,-96),(53,-51),(-13,46),(91,73),(32,-19),(42,-56),(4,22),(56,-55),(-52,-18),(-70,61),(77,-90),(-55,-98),(-5,56),(-4,-14),(43,-39),(82,83),(76,-48),(-97,-96),(65,18),(-42,1),(-41,9),(-81,24),(57,20),(13,-5),(34,63),(44,52),(-72,-70),(-78,-99),(-30,1),(83,89),(8,-19),(-22,-58),(90,81),(-55,79),(82,60),(-68,-32),(33,-2),(-84,91),(-23,14)]

+++ OK, passed 100 tests.
-}

----------------
-- Generators --
----------------
-- Adapted from lecture 2
genRel :: IO (Rel Int)
genRel = do 
            n <- getRandomInt 10
            k <- getRandomInt 10
            getRelL k n

-- Adapted from lecture 2
getRelL :: Int -> Int -> IO (Rel Int)
getRelL _ 0 = return []
getRelL k n = do
                x   <-  getRandomInt 100
                y   <-  getRandomInt 100
                rs  <- getRelL k (n-1)
                let r = (x,y)
                return (r:rs)

-- Adapted from lecture 2
genRels :: Int -> IO [Rel Int]
genRels n = do 
            k <- getRandomInt 10
            getRelsL k n

-- Adapted from lecture 2
getRelsL :: Int -> Int -> IO [Rel Int]
getRelsL _ 0 = return []
getRelsL k n = do
                r   <- genRel
                rs  <- getRelsL k (n-1)
                return (r:rs)

propTester_4_7_1 :: Show a => Int -> (Rel a -> Bool) -> [Rel a] -> IO ()
propTester_4_7_1 n prop []        = print ("Passed all tests")
propTester_4_7_1 n prop (c:cases) = do
                                      if(prop c) then do
                                        print ("Passed on: " ++ (show c))
                                        propTester_4_7_1 (n-1) prop cases
                                      else
                                        print ("Failed on: " ++ (show c))

propTester_4_7_2 :: Show a => Int -> (Rel a -> Rel a -> Bool) -> [(Rel a,Rel a)] -> IO ()
propTester_4_7_2 n prop []        = print ("Passed all tests")
propTester_4_7_2 n prop (c:cases) = do
                                      let (a,b) = c
                                      if(prop a b) then do
                                        print ("Passed on: " ++ (show a))
                                        propTester_4_7_2 (n-1) prop cases
                                      else
                                        print ("Failed on: " ++ (show a))
