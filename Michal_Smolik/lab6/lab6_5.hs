module Lab6_5 where

import Lecture6
import Lab6_4
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

testCarmichael = testListF 3 carmichael

{-
*Lab6_5> testCarmichael
294409
(0.21 secs, 3,757,984 bytes)

*Lab6_5> factors 294409
[37,73,109]
(0.00 secs, 328,064 bytes)
-}
