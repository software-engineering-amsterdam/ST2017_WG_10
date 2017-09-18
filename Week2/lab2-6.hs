--Implementing and testing ROT13 encoding - time: 30 mins
module Exercise6 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{- Implementing and testing ROT13 encoding
   ROT13 is a single letter substitution cipher that is used in online forums for hiding spoilers.
   See also www.rot13.com.
-}

{-First, give a specification of ROT13. -}
{-
	ROT13 is an encryption method for texts.
	If we have a String Str. We encrypt alphabetical characters only (a..z & A..Z).
	All other characters remain untouched (numbers, special characters .. etc .)
	Encryption method:
	Each alphabetical character is replaced by the alphabetical character that is located after 13 positions.
	During shifting, in case we reached the end of the alphabetical character list before the 13 characters are counted,
	we go back to the beginning and continue the shifting (rotation).
-}

{-Next, give a simple implementation of ROT13.-}

shift = 13


rotateChar :: Char -> Char
rotateChar c 
    |elem c ['a'..'z'] = chr.(+97).(`mod` 26).(+ (-84)).ord $ c
    |elem c ['A'..'Z'] = chr.(+65).(`mod` 26).(+ (-52)).ord $ c
    |otherwise = c
	
rot13Encrypt :: String -> String
rot13Encrypt xs = map (rotateChar) xs

{- GHCi: wiki
    *Exercise6> rot13Encrypt "Why did the chicken cross the road?"
	"Jul qvq gur puvpxra pebff gur ebnq?"
-}

{-Finally, turn the specification into a series of QuickCheck testable properties, and use these to test your implementation.-}

-- helper to execute the encryption twice on the same text
doubleRot :: String -> String
doubleRot cs = rot13Encrypt $ rot13Encrypt cs

--1
isSameDoubleRot13 :: String -> Bool
isSameDoubleRot13 cs = (rot13Encrypt $ rot13Encrypt cs) == cs

--2 we can take similar function as in exercise 5
-- can't be that same character is in the same position, it must have been rotated
checkSelf :: String -> Bool
checkSelf cs = and [c1 /= c2 | c1 <- cs, c2 <- rot13Encrypt cs,
					elem c1 ['A'..'z'] &&  elem c2 ['A'..'z']]

--3
hasSameLength :: String -> Bool
hasSameLength cs = length cs == length (rot13Encrypt cs)

{- GHCi:
		*Exercise6> quickCheck isSameDoubleRot13
		+++ OK, passed 100 tests.
		
		*Exercise6> quickCheck hasSameLength
		+++ OK, passed 100 tests.
-}

main = do
		print "Testing properties"
		print "1: isSameDoubleRot13"
		quickCheck isSameDoubleRot13
		print "2: checkSelf"
		quickCheck checkSelf
		print "3: hasSameLength"
		quickCheck hasSameLength
