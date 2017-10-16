-- time: 30 minutes
module Lab6_8
where

import Data.Char (ord, chr)
import Data.List
import System.Random
import Lecture6

-- lookup prime number that fulfil Rabin's algorithm, if x, otherwise look forward, and backword, otherwise keep searching forward
getPrime :: Integer -> IO Integer
getPrime x = do
    p <- primeMR 5 x; -- check x, k =5
    q <- primeMR 5 (pred x); -- check previous integer, k = 5
    l <- primeMR 5 (succ x); -- check next integer, k = 5
    if p then return x else if q then return (pred x) else if l then return (succ x)
    else getPrime (x+3) -- else keep searching forward

genPair :: IO (Integer, Integer)
genPair = do
    x <- getStdRandom (randomR (1, 1000))
    y <- getStdRandom (randomR (1, 1000))
    xp <- getPrime x
    yp <- getPrime y
    return (xp,yp)

main68 = do
    let msg = 12234 -- base 5 digits
    (m, n) <- genPair
    let publicKey  = rsaPublic m n
    let privateKey = rsaPrivate m n
    let encodeMsg  = rsaEncode publicKey msg -- encode original message
    let decodeMsg  = rsaDecode privateKey encodeMsg -- decode encoded message
    print (show msg ++ " " ++ show encodeMsg ++ " " ++ show decodeMsg)

{- 

*Lab6_8> main68
"15122 23950 15122"
(0.01 secs, 748,304 bytes)
*Lab6_8> main68
"15122 95734 15122"
(0.01 secs, 652,952 bytes)
*Lab6_8> main68
"15122 85382 15122"
(0.01 secs, 829,632 bytes)
*Lab6_8> main68
"15122 45279 15122"
(0.01 secs, 565,720 bytes)

-}