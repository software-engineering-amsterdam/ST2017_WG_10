module Lab1_7 where
import Data.List
import Test.QuickCheck  
import System.Random

--`luhn n` checks if the actual last digit is the same as the correct one, computed by `luhnstep`.
luhn :: Integer -> Bool
luhn a = (head(reverse (digits a))) == (luhnstep 0 $ tail $ reverse $ digits a)

--luhnstep(sum_so_far, remaining_digits) returns the correct check-number. Expects digits to be last-to-first.
luhnstep :: Integer -> [Integer] -> Integer
luhnstep m [] = m*9 `mod` 10
luhnstep m [x] 
    | x*2 < 10 = (m + 2*x)*9 `mod` 10
    | otherwise = (m + (2*x) `mod` 10 + 1)*9 `mod` 10
luhnstep m (x:y:ss)
    | x*2 < 10 = luhnstep ((m + 2*x)+y) ss
    | otherwise = luhnstep ((m + (2*x) `mod` 10 + 1)+y) ss

--digits(n) returns list of digits in `n`
digits :: Integer -> [Integer]
digits =  map (read . (:[])) . show

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = length(d) == 15 && (head (d) == 3) && (elem (d!!1) [4,7]) && luhn n
    where d = digits n

isMaster :: Integer -> Bool
isMaster n = length(d) == 16 && (head (d) == 5) && (elem (d!!1) [1..5]) && luhn n
    where d = digits n
isVisa :: Integer -> Bool
isVisa n = (elem (length(d)) [13,16,19]) && (head (d) == 4) && luhn n
    where d = digits n


{-Test should see, if any valid card number is properly validated, but also if there are no
false positives. That's why I implemented tester, that after being given a seed, will decide
if it will test for correct validation or for false positives. The test succeeds, if it 
successfuly validates a valid number, or if it rejects an invalid.

Randomness in this implementation might not be the best, because of Haskell's confusing
implementation of randomness. Also, invalid numbers are always generated with negative seed,
which might impact their generation. The same with valid cards. And finally, as the function
to find the checksum digit is the same in validation and generation, this test might not
find any faults in that implementation. However, it is impossible to generate valid card numbers
without it, so it's used anyway.

For other cards are the differences in tests minimal (only constants),
so I see little need to implement them too.
-}

--pick chooses random element of the list
pick :: [Integer] -> Int -> Integer
pick xs seed = xs !! fst ( randomR (0,length(xs)-1) (mkStdGen seed))

--generates valid American Express card
genAECard :: Int -> Integer
genAECard seed = cp*10 + (luhnstep 0 $ reverse $ digits cp)
    where cp = (pick ([34,37]) seed)*10^12 + fst(randomR (0,((10^12)-1)) (mkStdGen seed))

--generates invalid AE card
genWrongCard :: Int -> Integer
genWrongCard seed
    | luhn cp  = cp + (luhnstep 0 $ tail $ reverse $ digits cp)
    | otherwise = cp
    where cp = fst(randomR (0,(10^16+10^9)) (mkStdGen seed))

test :: Int -> Bool
test seed = if seed > 0 then isAmericanExpress $ genAECard seed
        else 
            not $ isAmericanExpress $ genWrongCard seed

--2.5 hours