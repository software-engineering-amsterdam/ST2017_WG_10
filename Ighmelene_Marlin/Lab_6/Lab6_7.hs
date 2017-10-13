module Lab6_7 where

import Data.List
import System.Random
import Lecture6

-- 180 min

{-
Ex 7: 
The recipe: take a prime p, and use the Miller-Rabin algorithm to check whether (2^p)−1 is also prime. 

1) Take a prime p
*Lab6_7> primes

2) Get (2^p)-1
*Lab6_7> map (\p -> pred $ 2^p) primes

3) Use the Miller-Rabin algorithm to check whether (2^p)−1 is also prime
*Lab6_7> testMersenneMillerRabin k (map (\p -> pred $ 2^p) primes)

The mers function in Lecture6 returns the first 25 Mersenne primes.
Those are used to verify that p is indeed a Mersenne prime.
*Lab6_7> isMersenne 3
True

*Lab6_7> getMersenne 3
1

*Lab6_7> testMersenneMillerRabin 1024 [3] 1

Probably prime (k = 1024):  p = 3
Mersenne prime:  1

Because the numbers are quite large, so even with k=1 it's take long to get pass the 20th number; 285542542228279613901563566102164008326164238644702889199247456602284400390600653875954571505539843239754513915896150297878399377056071435169747221107988791198200988477531339214282772016059009904586686254989084815735422480409022344297588352526004383890632616124076317387416881148592486188361873904175783145696016919574390765598280188599035578448591077683677175520434074287726578006266759615970759521327828555662781678385691581844436444812511562428136742490459363212810180276096088111401003377570363545725120924073646921576797146199387619296560302680261790118132925012323046444438622308877924609373773012481681672424493674474488537770155783006880852648161513067144814790288366664062257274665275787127374649231096375001170901890786263324619578795731425693805073056119677580338084333381987500902968831935913095269821311141322393356490178488728982288156282600813831296143663845945431144043753821542871277745606447858564159213328443580206422714694913091762716447041689678070096773590429808909616750452927258000843500344831628297089902728649981994387647234574276263729694848304750917174186181130688518792748622612293341368928056634384466646326572476167275660839105650528975713899320211121495795311427946254553305387067821067601768750977866100460014602138408448021225053689054793742003095722096732954750721718115531871310231057902608580607

But up to that point, all the primes where (2^p)-1 was a prime, were identified as Mersenne primes

-}
main67 :: IO ()
main67 = doMain67 0

---------------
-- Iterators --
---------------
testMersenneMillerRabin :: Int -> [Integer] -> Int -> IO ()
testMersenneMillerRabin _ [] _ = putStr ""
testMersenneMillerRabin k (x:xs) v = do
                                      p <- primeMR k x
                                      if p then do
                                        putStrLn ("\nProbably prime (k = "++(show k)++"):  p = " ++ (show x)) 
                                        putStrLn ("Mersenne prime:  " ++ (show $ getMersenne x) )
                                        testMersenneMillerRabin k xs v
                                      else do
                                        if (v == 1) then putStrLn ("\nComposite: p = " ++ (show x)) else putStr ""
                                        testMersenneMillerRabin k xs v

-------------
-- Helpers --
-------------
-- List of Mersenne primes from Lecture6 (1 - 25)
mers' :: [Integer]
mers' = map mers [1..25]

-- Check if p is a known Mersenne prime
isMersenne :: Integer -> Bool
isMersenne p = (length $ elemIndices p mers') == 1

-- Get the index of the Mersenne prime or -1 if it's not a (known) Mersenne prime
getMersenne :: Integer -> Int
getMersenne p 
  | isMersenne p  = succ $ head $ elemIndices p mers'
  | otherwise     = -1

-- If verbose, only print cases which return "Probably prime" otherwise print all test cases
doMain67 :: Int -> IO ()
doMain67 verbose = do
  putStrLn ("Looking for Marsenne primes")
  let k = 1024
  let xs = map (\x -> pred (2^x)) primes
  testMersenneMillerRabin k xs verbose
  
-- Print all test cases
verboseMain67 :: IO ()
verboseMain67 = doMain67 1

