-- Implementing and testing IBAN validation - time:
module Exercise7 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{- implement iban validator -}


{-http://www.xe.com/ibancalculator/countrylist/-}
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

iban :: String -> Bool
iban [] = False
iban cs = hasIbanMaxLength cs
          &&
          hasValidCountryCode cs
		  &&
		  hasValidIbanLengthPerCountry cs
		  &&
		  checkSum cs
	
{- GHCi:
    *Exercise7> iban "NL91ABNA0417164300"
    True
-}

{- TODO: optimizing code, comments, testing & testing report -}