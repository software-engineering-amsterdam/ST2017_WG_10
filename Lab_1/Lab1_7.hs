module Lab1 where
import Data.List
import Test.QuickCheck
import Data.String.Utils

-- 180 min
-- I don't know how to test correctnes, or I should say I don't know if the data I'm testing is indeed correct.
-- What I could do is generate numbers based on my  check

toDigits :: Integer -> [Int]
toDigits n = [read [x] :: Int | x <- show n]

getDouble :: Int -> Int
getDouble n = sum (toDigits (read (show (n * 2)) :: Integer))

getDoubles :: [Int] -> [Int]
getDoubles xs = [getDouble (rxs !! (x-1)) | x <- [1..(length xs)], odd x]
                where rxs = reverse xs

getSingles :: [Int] -> [Int]
getSingles xs = [rxs !! (x-1) | x <- [1..(length xs)], even x]
                where rxs = reverse xs

getSum :: [Int] -> Int
getSum xs = sum [sum (getDoubles xs), sum (getSingles xs)]

getCheckDigit :: [Int] -> Int
getCheckDigit xs = mod ( 9 * (getSum xs) ) 10

luhn :: Integer -> Bool
luhn n =  getCheckDigit (init xs) == last xs
          where xs = toDigits n

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = luhn n && iins && l == 15
                      where 
                        s = show n
                        l = length s
                        iins = elem (read (take 2 s)) [34,37]

isMaster n = luhn n && ( iins1 || iins2 ) && l == 16
                      where 
                        s     = show n
                        l     = length s
                        iins1 = elem (read (take 2 s)) [51..55]
                        iins2 = elem (read (take 4 s)) [2221..2720]

isVisa n = luhn n && iins && (l == 13 || l == 16 || l == 19)
                      where 
                        s     = show n
                        l     = length s
                        iins  = take 1 s == ['4']

amex =  [
          330123456789016,
          340123456789014,
          350123456789011,
          370123456789017,
          380123456789015,
          375677715995215,
          373410420058927,
          340143311094962
        ]

mc = [
        5001234567890127,
        5101234567890126,
        5301234567890124,
        5501234567890122,
        5601234567890121,
        2220012345678903,
        2221012345678902,
        2500012345678904,
        2720012345678908,
        2721012345678907,
        5397757314467510,
        2221003345366853,
        5137051272115591
      ]

visa = [
        3012345678901231,
        4012345678901239,
        5012345678901236,
        4929379295715930,
        4929786939297259,
        4716996461778336468
      ]
