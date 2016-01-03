module Main where

import Lib
b = 0
bd1  = [ 
          b, b, b, b, b, b, b, b, b
        , 1, b, b, b, b, b, b, b, b
        , b, b, b, b, b, b, b, b, b
        , b, b, b, b, b, b, b, b, b 
        , b, b, b, b, b, b, b, b, b
        , b, b, b, b, b, b, b, b, b
        , b, b, b, b, b, b, b, b, b
        , b, b, b, b, b, b, b, 7, b
        , 8, b, b, b, b, b, b, b, b]  :: Soduku

pre_sol_bd1_2 = [
            0,7,4,8,9,1,3,6,5       -- two step from solution
          , 1,3,8,5,2,6,4,9,7
          , 6,5,9,4,7,3,2,8,1
          , 3,2,1,9,6,4,0,5,8
          , 9,8,5,1,3,7,6,4,2
          , 7,4,6,2,8,5,9,1,3
          , 4,6,2,7,5,8,1,3,9
          , 5,9,3,6,1,2,8,7,4
          , 8,1,7,3,4,9,5,2,6 ]      :: Soduku    

main :: IO ()
main = printSodoku $ solve_Recursion bd1
