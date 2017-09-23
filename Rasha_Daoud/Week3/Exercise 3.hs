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

-- morgan law needed for the conversion
applyMorganLaw :: Form -> Form -> Form
applyMorganLaw frm frm'                     = Cnj [frm, frm']
applyMorganLaw (Cnj (frm:frms)) frm'        = Cnj [applyMorganLaw frm frm', applyMorganLaw (Cnj frms) frm']
applyMorganLaw frm (Cnj (frm':frms))        = Cnj [applyMorganLaw frm frm', applyMorganLaw frm (Cnj frms)]

-- converts a proposational form into CNF
convert2CNF :: Form -> Form
convert2CNF form = cnf $ nnf $ arrowfree form -- lecture 3 slides

{---------------------------------------------------Testing-----------------------------------------------------------}

{- We can pass any form to the conversion function.
    GHCi:
	convert2CNF (Dsj [Neg (Prop 3), Impl (Prop 1) (Prop 4)])
	*(-3 +((1==>4)))
	
	convert2CNF (Dsj [Neg (Prop 3), Dsj [(Prop 1), Dsj [(Prop 3), (Prop 4)]]])
    *(-3 +(+(1 +(3 4))))
	
	convert2CNF (Dsj [Impl (Prop 1) (Prop 2), Neg (Prop 3)])
    *(*(-1 +(2)) +(-3))

 Furthermore, we can test wether the conversion went fine, if the properties of the original statement still holds with the converted one.
 ** Let's take the first example: (Dsj [Neg (Prop 3), Impl (Prop 1) (Prop 4)])
      converting it to CNF gave:
	      *(-3 +((1==>4))) which is equivalent to: 
               Cnj [Dsj [(Neg (Prop1)), Impl (Prop 1) (Prop4)]]

    - original form  
	  *Exercise1> tautology (Dsj [Neg (Prop 3), Impl (Prop 1) (Prop 4)])
      False
      *Exercise1> satisfiable (Dsj [Neg (Prop 3), Impl (Prop 1) (Prop 4)])
      True

    -converted form
      *Exercise1> tautology (Cnj [Dsj [(Neg (Prop 1)), Impl (Prop 1) (Prop 4)]])
      False
      *Exercise1> satisfiable (Cnj [Dsj [(Neg (Prop 1)), Impl (Prop 1) (Prop 4)]])
      True

-}
