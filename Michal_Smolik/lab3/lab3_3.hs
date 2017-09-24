module Lab3_3 where
import Data.List (intercalate, group, sort)
import Lecture3

--my own type for CNF, to make the work easies by separating processed and unprocessed parts of form type-wise
--list of lists of pairs, every inner list is a disjunction and Bool value signifies if the literal is or isn't negated
type CNF = [[(Name, Bool)]]

cnfToForm :: CNF -> Form
cnfToForm cnf = Cnj (map dsj cnf) where
    dsj f = Dsj (map formify f)
    formify (x,True) = Prop x
    formify (x,False) = Neg(Prop x) 

-- change Cnj and Dsj so that they are applied only on pairs, to reduce number of cases for the converter
-- input is arrow-free formula, output is a formula where every Dsj and Cnj is applied to exactly two subformulas
associate :: Form -> Form
associate (Prop f) = Prop f
associate (Neg f) = Neg (associate f)
associate (Cnj [x]) = associate x
associate (Dsj [x]) = associate x
associate (Cnj (x:xs)) = Cnj [associate x, associate (Cnj xs)]
associate (Dsj (x:xs)) = Dsj [associate x, associate (Dsj xs)]


cnf :: Form -> Form
cnf = cnfToForm . deduplicate . map deduplicate . convert . associate . arrowfree where
    -- Conjunction of CNFs is concatenation of CNFs
    convert (Cnj [x, y]) = convert x ++ convert y
    -- Disjunction is reduced by distributive property
    convert (Dsj [x, y]) = combine (convert x) (convert y)
    -- Neg requires de Morgan law
    convert (Neg x) = negate (convert x)
    -- trivial case
    convert (Prop z) = [[(z, True)]]
    -- combines Disjunctions
    combine = concatMap . flip (map . (++))
    -- applies de Morgan law
    negate = foldr1 combine . map (map (\(x, y) -> [(x, not y)]))
    -- sort the list and remove duplicates to make it look nice
    deduplicate :: Ord a => [a] -> [a]
    deduplicate = map head . group . sort

--time spent: 4h
