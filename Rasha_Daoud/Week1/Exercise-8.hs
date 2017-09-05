-- Exercise 8 -time: 1,25 hour
-- (I had to read a bit, to realize that I can compare values of data type- (Graham Huton book))
module Exercise8 where
import Data.List
import Test.QuickCheck

{-- Some statements:
		Matthew: Carl didn't do it, and neither did I.
		Peter It was Matthew or it was Jack.
		Jack Matthew and Peter are both lying.
		Arnold Matthew or Peter is speaking the truth, but not both.
		Carl What Arnold says is not true.
		
	Important:
	    Teacher says: three of these boys always tell the truth,
        which means: we have 3 as the length of honest list.
--}


data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]


-- Implement accuses using pattern matching & statements (Graham Huton book)
accuses :: Boy -> Boy -> Bool
--Matthew: Carl didn't do it, and neither did I.
accuses Matthew boy' = if ((boy' == Carl) || (boy' == Matthew)) then False else True

--Peter It was Matthew or it was Jack.
accuses Peter boy' = if ((boy' == Jack) || (boy' == Matthew)) then True else False

--Jack Matthew and Peter are both lying. Therefore their truth is false!
accuses Jack boy' = not( (accuses Matthew  boy') || (accuses Peter boy') )

--Arnold Matthew or Peter is speaking the truth, but not both.
accuses Arnold boy' =  not(accuses Matthew boy' == accuses Peter boy') -- because data Boy is orderd (Graham Huton book)

--Carl What Arnold says is not true.
accuses Carl boy' = not( accuses Arnold  boy')



-- Implement accusers using list comprehension (GH book)
-- Get boys who accuses the input boy
accusers :: Boy ->[Boy]
accusers b = [b' | b'<- boys, accuses b' b]

-- list of guilty boys, it should contain one boy, we have 3 honest people
guilty :: [Boy]
guilty =[boy' | boy' <- boys, length (accusers boy') == 3]

-- list of honest boys (fact = it should have 3 boys according to the teacher)
honest :: [Boy]
honest =[boy' | boy' <- boys, gboy <- guilty, (accuses boy' gboy)] -- check who accuses the guilty guy, he must be honest

{-- GHCi: solution verification
		*Exercise8> guilty
		[Jack]
		*Exercise8> honest
		[Matthew,Peter,Carl]
--}




-- these functions are for testing purposes
countAccusesPerBoy :: Boy -> Int
countAccusesPerBoy b = sum [1 | b' <- boys, accuses b' b]
countAccusesForBoys :: [Boy] -> [(Int,Boy)]
countAccusesForBoys bx = [(countAccusesPerBoy b, b) | b <-bx]
--GHCi: countAccusesForBoys boys = [(Matthew,3),(Peter,1),(Jack,1),(Arnold,1),(Carl,0)]
