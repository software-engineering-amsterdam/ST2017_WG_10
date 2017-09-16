-- Implementing and testing IBAN validation - time: 50 mins
module Exercise7 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{- implement iban validator -}


{-http://www.xe.com/ibancalculator/countrylist/-}
-- Let us consider the support for 4 countries for now
countryCodesLength = [("NL", 18), ("DE", 22), ("FR", 27), ("ES", 24)]


{- global length property -}
hasIbanMaxLength :: String -> Bool
hasIbanMaxLength cs = length cs <=34 -- all iban around the world has 34 or less characters

{- customized property: if we have a list of defined country code/iban length per country -}
hasValidIbanLengthPerCountry :: String -> Bool
hasValidIbanLengthPerCountry cs | getLengthCountryCode cs == 0   = False
                                | otherwise                      = genericLength cs == getLengthCountryCode cs


{-country code property - first two chars -}
hasValidCountryCode :: String -> Bool
hasValidCountryCode cs =  elem (map toUpper (take 2 cs)) [fst x | x<-countryCodesLength] -- toUpper, in case iban is lower case

{- checks whether the country code in the prefix is in the defined list, and get the iban length in that country -}
getLengthCountryCode :: String -> Integer
getLengthCountryCode cs | result == [] = 0
                        | otherwise = head result
						 where
						   result = [snd x | x <- countryCodesLength, map toUpper (take 2 cs) == fst x]


{- move first 4 chars to the end of the iban -}
move4ToEnd :: String -> String
move4ToEnd cs = drop 4 cs ++ take 4 cs


{-replace each char with its two digits number using ord on char -}
chars2digits :: String -> [Int]
chars2digits cs = [ord (toUpper c) - 55 | c <- move4ToEnd cs, elem (toUpper c) ['A'..'Z']]

{- sum remainder 97 should be 1 property-}
checkSum :: String -> Bool
checkSum cs = mod (sum $ chars2digits cs) 97 == 1

{- helper, to remove spaces -}
skipSpaces :: String -> String
skipSpaces cs = [c | c <- cs, not (ord c == 32)]

iban :: String -> Bool
iban [] = False
iban cs = hasIbanMaxLength cs'
          &&
          hasValidCountryCode cs'
		  &&
		  hasValidIbanLengthPerCountry cs'
		  &&
		  checkSum cs'
		  where cs' = skipSpaces cs
	
{- GHCi:
    *Exercise7> iban "NL91ABNA0417164300"
    True
	*Exercise7> iban "NL91 ABNA 0417 1643 00"
	True
-}

{- Next, test your implementation using some suitable list of examples. -}

ibans :: [(String, Bool)]
ibans   =       [
					("NL91ABNA0417164300", True),
					("NL18ABNA0484869868",True),
					("ES1020903200500041045040", False),
					("DE12500105170648489890", False),
					("FR7630066100410001057380116", False),
					("NL91ABNA0417164301", True), 
					("NL18ABNA0484869863", True),
					("ES10209032005000410450", False),
					("FR2630066100210001057", False),
					("DE12500",False)
				]

testCorrectness :: Bool
testCorrectness = and [iban (fst c) == snd c| c <- ibans]

main = do
		print "Test correctness of the validator:"
		print  testCorrectness

{- GHCi:
	*Exercise7> main
	"Test correctness of the validator:"
	True
}