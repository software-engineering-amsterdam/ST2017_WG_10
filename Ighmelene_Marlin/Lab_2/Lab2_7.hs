module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 120 min
iban :: String -> Bool
iban s = do
            let stripped = [c | c <- s, c /= ' ']
            let cc = take 2 stripped
            let cd = take 2 (drop 2 stripped)
            let bban = drop 4 stripped
            if(isAlphaNumCS cc && isAlphaNumCI bban && bbanLength cc == length bban) then 
              do
              let shifted = first4toEnd stripped
              let subbed  = charSubstitutions shifted
              let intval  = stringToInt subbed
              mod (mod intval 97) 10 == 1
            else
              False
            
-- Check that the total IBAN length is correct as per the country. If not, the IBAN is invalid
bbanLength :: [Char] -> Int
bbanLength cc | cc == "MT" = 27
              | cc == "QA" = 25
              | cc == "PT" = 21
              | cc == "BE" = 12
              | cc == "NO" = 11
              | elem cc ["JO","KW","MU"] = 26
              | elem cc ["AL","AZ","CY","HU","LB","PL"] = 24
              | elem cc ["FR","GR","IT","MC","SM"] = 23
              | elem cc ["IS","TR"] = 22
              | elem cc ["AD","CZ","MD","PK","RO","SA","SK","ES","SE","TN"] = 20
              | elem cc ["GI","IL","AE"] = 19
              | elem cc ["BH","BG","GE","DE","IE","ME","RS","GB"] = 18
              | elem cc ["HR","LV","LI","CH"] = 17
              | elem cc ["AT","BA","EE","LT","LU"] = 16
              | elem cc ["MK","SI"] = 15
              | elem cc ["DK","FO","FI","GL","NL"] = 14
              | otherwise = 30

-- Move the four initial characters to the end of the string
first4toEnd :: [Char] -> [Char]
first4toEnd s = drop 4 s ++ take 4 s

-- Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35
charSubstitutions :: [Char] -> [Char]
charSubstitutions [] = ""
charSubstitutions (x:xs) = charSubstitution x ++ charSubstitutions xs

charSubstitution :: Char -> [Char]
charSubstitution c   |  between (ord c) 65 90   = show ((ord c) - 55)
                     |  between (ord c) 97 122  = show ((ord c) - 87)
                     |  otherwise  = [c]

-- Interpret the string as a decimal integer and compute the remainder of that number on division by 97
stringToInt ::[Char] -> Integer
stringToInt s = read s :: Integer
            
between :: Int -> Int -> Int -> Bool
between n x y = if (x <= n && n <= y) then True else False

isAlphaNumCS :: [Char] -> Bool
isAlphaNumCS s = do
                  let alphanum = ['0'..'9']++['A'..'Z']
                  let valid = filter (\c -> elem c alphanum) s
                  s == valid
                  
isAlphaNumCI :: [Char] -> Bool
isAlphaNumCI s = do
                  let alphanum = ['0'..'9']++['A'..'Z']++['a'..'z']
                  let valid = filter (\c -> elem c alphanum) s
                  s == valid
                  
ibans :: [[Char]]
ibans = [
    "AL47212110090000000235698741",
    "AD1200012030200359100100",
    "AT611904300234573201",
    "AZ21NABZ00000000137010001944",
    "BH67BMAG00001299123456",
    "BE62510007547061",
    "BA391290079401028494",
    "BG80BNBG96611020345678",
    "HR1210010051863000160",
    "CY17002001280000001200527600",
    "CZ6508000000192000145399",
    "DK5000400440116243",
    "EE382200221020145685",
    "FO9754320388899944",
    "FI2112345600000785",
    "FR1420041010050500013M02606",
    "GE29NB0000000101904917",
    "DE89370400440532013000",
    "GI75NWBK000000007099453",
    "GR1601101250000000012300695",
    "GL5604449876543210",
    "HU42117730161111101800000000",
    "IS140159260076545510730339",
    "IE29AIBK93115212345678",
    "IL620108000000099999999",
    "IT40S0542811101000000123456",
    "JO94CBJO0010000000000131000302",
    "KW81CBKU0000000000001234560101",
    "LV80BANK0000435195001",
    "LB62099900000001001901229114",
    "LI21088100002324013AA",
    "LT121000011101001000",
    "LU280019400644750000",
    "MK07250120000058984",
    "MT84MALT011000012345MTLCAST001S",
    "MU17BOMM0101101030300200000MUR",
    "MD24AG000225100013104168",
    "MC9320052222100112233M44555",
    "ME25505000012345678951",
    "NL39RABO0300065264",
    "NO9386011117947",
    "PK36SCBL0000001123456702",
    "PL60102010260000042270201111",
    "PT50000201231234567890154",
    "QA58DOHB00001234567890ABCDEFG",
    "RO49AAAA1B31007593840000",
    "SM86U0322509800000000270100",
    "SA0380000000608010167519",
    "RS35260005601001611379",
    "SK3112000000198742637541",
    "SI56191000000123438",
    "ES8023100001180000012345",
    "SE3550000000054910000003",
    "CH9300762011623852957",
    "TN5910006035183598478831",
    "TR330006100519786457841326",
    "AE070331234567890123456",
    "GB29RBOS60161331926819"
  ]