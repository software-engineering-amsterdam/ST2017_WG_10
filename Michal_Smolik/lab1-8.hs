module Lab1_8 where

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

--returns True, if exactly one input is True
xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False
{-
Matthew: Carl didn't do it, and neither did I.

Peter It was Matthew or it was Jack.

Jack Matthew and Peter are both lying.

Arnold Matthew or Peter is speaking the truth, but not both.

Carl What Arnold says is not true.
-}



{-
Idea of this implementation is to evaluate each statement, if it is true, based on suspect boy (boy, whose guilt are we assuming for the moment).
First, we declare the boys' statements in statement_true(who_said_it, suspect_boy). That will return true, if their
statement is true. Then, we evaluate the statements for each suspect boy. If there are exactly two false statements,
the boy is guilty, because every condition is satisfied. Otherwise, he is not guilty.
Then, we find liars for each possibly guilty boy by filtering through their statements.
-}
--statement_true(who_said_it, suspect_boy) 
statement_true :: Boy -> Boy -> Bool
statement_true Matthew Matthew = False
statement_true Matthew Carl = False
statement_true Matthew _ = True

statement_true Peter Matthew = True
statement_true Peter Jack = True
statement_true Peter _ = False

statement_true Jack b = (not (statement_true Matthew b)) && (not (statement_true Peter b))

statement_true Arnold b = xor (statement_true Matthew b) (statement_true Peter b)

statement_true Carl b = not $ statement_true Arnold b

--returns true/false values for each boy's statement for suspect boy
eval :: Boy -> [Bool]
eval b = map (\g -> statement_true g b) boys

count_false :: [Bool] -> Integer
count_false = foldl (\i v -> if not v then i + 1 else i) 0

--checks for which suspect boy is the number of false statements exactly true (list, in case there are multiple solutions).
guilty :: [Boy]
guilty = filter (\b -> count_false(eval b) == 2) boys

--for each guilty boy finds liars in that instance (list of lists, in case there are multiple solutions)
liars :: [[Boy]]
liars = map (\g -> (filter (\b -> not (statement_true b g)) boys)) guilty
--1.5h