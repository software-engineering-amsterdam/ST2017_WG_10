
module Lab5_3

where 

import Data.List
import System.Random
import Lecture5

hasSingleSolution :: Sudoku -> Bool
hasSingleSolution s = 1 == length(solveNs(initNode(sud2grid s)))

isMinimal :: Sudoku -> Bool
isMinimal s = (hasSingleSolution s) && (not $ any hasSingleSolution [eraseS s (i,j) |
                                                                    i <- [1..9], j <- [1..9],
                                                                    s (i,j) > 0 ])

canBeRemoved :: Sudoku -> [(Row, Column)]
canBeRemoved s = [(i,j)| i<-[1..9], j<-[1..9], s(i, j) > 0, hasSingleSolution (eraseS s (i,j))]

g1 = [[4,9,0,0,3,0,8,0,6],[0,0,8,0,9,0,3,0,0],[5,0,0,0,0,0,0,0,0],[8,0,0,0,0,6,0,2,0],[0,0,0,4,0,0,0,3,5],[0,6,5,0,0,0,0,0,7],[9,8,0,0,0,0,0,6,0],[0,0,4,0,0,0,0,0,0],[1,0,0,2,0,0,0,0,9]]
s1 = grid2sud g1