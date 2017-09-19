-- converting formulas into CNF -time: 1 hour
module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import Data.Char
import Lecture3

-- intermediate convertor from proposational form into CNF, we apply it after we remove arrows and then apply nnf - lecture 3 slides
cnf :: Form -> Form
cnf (Prop n)       = Prop n
cnf (Neg (Prop n)) = Neg (Prop n)
cnf (Neg frm)      = Neg (cnf frm)
cnf (Dsj frms)     = if (length frms) == 0 then (Dsj []) else if (length frms) == 1 then do (cnf (head frms))
                     else (applyMorganLaw (cnf (head frms)) (Dsj (tail frms)))
cnf (Cnj frms)     = Cnj (map cnf frms)
cnf _              = Cnj []

-- morgan law
applyMorganLaw :: Form -> Form -> Form
applyMorganLaw frm frm'                     = Cnj [frm, frm']
applyMorganLaw (Cnj (frm:frms)) frm'        = Cnj [applyMorganLaw frm frm', applyMorganLaw (Cnj frms) frm']
applyMorganLaw frm (Cnj (frm':frms))        = Cnj [applyMorganLaw frm frm', applyMorganLaw frm (Cnj frms)]

-- converts a proposational form into CNF
convert2CNF :: Form -> Form
convert2CNF form = cnf $ nnf $ arrowfree form -- lecture 3 slides

{---------------------------------------------------Testing-----------------------------------------------------------}
{- TODO -}

{- GHCi:
	convert2CNF (Dsj [Neg (Prop 3), Impl (Prop 1) (Prop 4)])
	*(-3 +((1==>4)))
	
	convert2CNF (Dsj [Neg (Prop 3), Dsj [(Prop 1), Dsj [(Prop 3), (Prop 4)]]])
    *(-3 +(+(1 +(3 4))))
	
	convert2CNF (Dsj [Impl (Prop 1) (Prop 2), Neg (Prop 3)])
    *(*(-1 +(2)) +(-3)
-}
