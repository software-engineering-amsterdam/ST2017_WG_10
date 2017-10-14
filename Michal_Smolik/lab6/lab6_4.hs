module Lab6_4 where

import Lecture6

testComposites = [(x, n)| n <- composites, x <- [2..n], exM x (n-1) n == 1 ]

testComposites' = snd $ head $ filter (\(x,y) -> x<=3) testComposites



testListF :: Int -> [Integer] -> IO(Integer)
testListF k (x:xs) = do
    v <- primeTestsF k x
    if v then return x else testListF k xs

runTComp = testListF 3 composites

{-
*Lab6_4> runTComp
561
(0.17 secs, 7,432,592 bytes)

*Lab6_4> runTComp
1105
(0.04 secs, 14,503,976 bytes)

*Lab6_4> runTComp
2465
(0.09 secs, 39,860,392 bytes)

*Lab6_4> runTComp
45
(0.00 secs, 332,320 bytes)
-}