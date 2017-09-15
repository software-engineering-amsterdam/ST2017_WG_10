--Implementing and testing ROT13 encoding - time: 
module Exercise6 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{- Implementing and testing ROT13 encoding
   ROT13 is a single letter substitution cipher that is used in online forums for hiding spoilers.
   See also www.rot13.com.
   First, give a specification of ROT13.
   Next, give a simple implementation of ROT13.
   Finally, turn the specification into a series of QuickCheck testable properties, and use these to test your implementation.
-}

shift = 13

rotateChar :: Char -> Char
rotateChar c      | elem c ['a'..'z'] = chr (97 + (mod (((ord c) - 97) + shift) 26)) -- ord 'a' = 97 & we have 26 chars in ['a'..'z']
				  | elem c ['A'..'Z'] = chr (65 + (mod (((ord c) - 65) + shift) 26)) -- ord 'A' = 65 & we have 26 chars in ['A'..'Z']
		          | otherwise = c -- return it as it is


rot13Encrypt :: String -> String
rot13Encrypt cs = map rotateChar cs

{- GHCi:
    *Exercise6> rot13Encrypt "Why did the chicken cross the road?"
	"Jul qvq gur puvpxra pebff gur ebnq?"
-}
