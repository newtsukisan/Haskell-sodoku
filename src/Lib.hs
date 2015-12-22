module Lib
 where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
type Soduku  = [Int]
type Row     = Int
type Col     = Int
type Index   = Int
type Rows    = [Int]
type Cols    = [Int] 
{-La idea es crear una estructura que nos permita resolver un soduko.
Cada uno de las filas y de las columnas tiene que contener solo una vez cada una de las cifras.
-} 

-- is a solution when all ciphers in columns and in rows are diferents 
-- is a solution when all ciphers are diferents of zero, (no empty values)
isSolution :: Soduku -> Bool
isSolution sdk = noempty && alldiferents
    where noempty      = (length $ filter (==0) sdk) == 0
          alldiferents = True

-- Obtain col and row index from index of list
getRindex :: Index -> Row
getRindex index  = index `div` 9

getCindex :: Index -> Col
getCindex index  = index `rem` 9

getIndex :: Row -> Col -> Index
getIndex r c = 9 * r + c

--Obtain one element by row and column
--getElement :: Soduku -> Row -> Col -> Integer
getElement sdk row col = sdk !! (getIndex row col)

-- is need to obtain rows and columns 
-- rows dividing by 9 
getRows :: Soduku -> [Rows]
getRows sdk = [] 


getCol  :: Soduku -> [Cols] 
getCol sdk = []