module Lab4_8 where

import Lab4_5
import Lab4_6

main = do
    print(show $ trClos $ symClos [(1,0),(1,2)])
    print(show $ symClos $ trClos [(1,0),(1,2)])