module Lib
 where

--import Data.Set (Set)
import qualified Data.Set as Set

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
dupla_0          :: [Int] -> [[Int]]
dupla_0 (x:xs)   = [[x,y] | y <- xs]
duplas           :: [Int] -> [[Int]]
duplas  [x]      = [[x]]
duplas  (x:y:[]) = [[x,y]]
duplas t@(x:ys)  = dupla_0 t ++ duplas ys

-- is a solution when all ciphers in columns and in rows are diferents 
-- is a solution when all ciphers are diferents of zero, (no empty values)
isSolution :: Soduku -> Bool
isSolution sdk = noempty && isValid sdk
    where noempty      = (length $ filter (== 0) sdk) == 0  -- no elements equals zero


-- Is it a valid soduku, could be a transition or a final solution
-- without zeros all elements in a sodoku must be diferent in each row and in each column
isValid :: Soduku -> Bool
isValid sdk =  rowsWell && colsWell
  where rowsWell =  all (==True) $  map areEquals rows   -- in all rows elements are diferents
        colsWell =  all (==True) $  map areEquals cols   -- in all colums elements are diferents
        rows     = map (filter (/=0)) (getRows sdk)      -- taking off zeros
        cols     = map (filter (/=0)) (getCols sdk)      -- taking off zeros

-- Check if all elements of a list are diferents
areEquals :: [Int] -> Bool
areEquals [x]   = True                       -- only an element, no problem
areEquals lista = (length $ filter (\ [x,y] -> x == y) $ duplas lista) == 0   -- more than one element
-- negate areEquals for simplicity
notEquals  :: [Int] -> Bool
notEquals lista = not $ areEquals lista
-- Obtain col and row index from index of list
getRindex :: Index -> Row
getRindex index  = index `div` 9

getCindex :: Index -> Col
getCindex index  = index `rem` 9

getIndex :: Row -> Col -> Index
getIndex r c = 9 * r + c

-- Obtain one element by row and column
-- begining in zero index
getElement :: Soduku -> Row -> Col -> Int
getElement sdk row col = sdk !! (getIndex row col)

-- is need to obtain rows and columns 
-- rows dividing by 9 
getRows :: Soduku -> [Rows]
getRows []  = []
getRows sdk = [(take 9 sdk)] ++  (getRows $ drop 9 sdk) 

-- all elements in a column have the same column index (...)
getCols  :: Soduku -> [Cols] 
getCols sdk = map get [0..8]
    where parejas = zip sdk (map getCindex [0..80]) 
          get n   = [ele |(ele, col) <- parejas, col == n] 


-- from a initial sodoku we must refill the 0 with numbers.
-- first creation simple sustitution with any other checking
-- get elements which are zero.
-- get rows of that elements.
-- only once get posibilities of that columns
-- try in zero position one of the posibilities.
-- for that taking into account only valids ones (poda)
-- podríamos sustituir solo uno de los valores de los ceros. 
-- De momento todos parecen conducir al mismo.
nextStep :: Soduku -> [Soduku]
nextStep sdk = [sodoku 
                 | index <- indexes                     -- where we have zeros
                 , x     <- [1..9]                      -- posibles substituions
                 , let sodoku = setElement index sdk x  -- substitution of possible values
                 , isValid sodoku                       -- filtering, only valids ones
               ]
      where indexes = getZeroIndexes sdk 

-- Only take one element of all which are zero.
nextStep1 :: Soduku -> [Soduku]
nextStep1 sdk  = [sodoku 
                 | x  <- [1..9]                         -- posibles substituions   
                 , let sodoku = setElement index sdk x  -- substitution of possible values
                 , isValid sodoku                       -- filtering, only valids ones
               ]                                        -- end list comprehensions
     where index = head $ getZeroIndexes sdk            -- index for looking up

-- Only take one element of all which are zero.
nextStep2 :: Soduku -> [Soduku]
nextStep2 sdk  = [sodoku 
                 | x  <- [1..9]                         -- posibles substituions   
                 , let sodoku = setElement index sdk x  -- substitution of possible values
                 , isValid sodoku                       -- filtering, only valids ones
               ]                                        -- end list comprehensions
     where index = head $ getZeroIndexes sdk            -- index for looking up 


-- taking a list of sodukus for each one:
-- create nextStep.
-- Check if any one is solution
-- Finish when we have a solution
-- Finish when empty if no solution
-- Finish when no more empty sodokus
solve sodokus solutions 
   | solutions /= []  = solutions
   | hasZeros sodokus = solve nextGeneration sols
   | sodokus == []    = []
   where  nextGeneration = concat [ nextStep sdk | sdk <-sodokus]
          sols           = filter isSolution nextGeneration
          hasZeros  sdks = any (==True) $ map (any (==0)) sdks

-- taking a list of sodukus for each one:
-- create nextStep.
-- Check if any one is solution
-- Finish when we have a solution
-- Finish when empty if no solution
-- Finish when no more empty sodokus
solve' sodokus solutions 
   | solutions /= []  = solutions
   | hasZeros sodokus = solve nextGeneration sols
   | sodokus == []    = []  
   where  nextGeneration = concat [ nextStep1 sdk | sdk <-sodokus]
          sols           = filter isSolution nextGeneration
          hasZeros  sdks = any (==True) $ map (any (==0)) sdks
            
-- given index sdk structure and new value 
-- obtain a new sdk structure with that value in specific position
setElement :: Int -> Soduku -> Int -> Soduku      
setElement n sdk x  = (take n sdk) ++ [x] ++ (drop (n + 1) sdk )

       
-- get index of elements which are zero values.
getZeroIndexes :: Soduku -> [Int]
getZeroIndexes sdk = [index | (ele,index) <- zip sdk [0..], ele == 0]

-- print Sodoku
printSodokus []         = return ()        
printSodokus (sdk:sdks) = do
    print "imprimiendo ....." 
    printSodoku  sdk
    printSodokus sdks
printSodoku sodoku =  putSodoku $ getRows sodoku 
putSodoku [] = return ()
putSodoku (x:xs) = do
    print x
    putSodoku xs


-- [1,0,0,5,0,0,0,9,0] -> todas las posibilidades que son los numeros que podríamos poner en los ceros
-- 2 3 4 6 7 8 
--getPosibilities :: [Int] -> [Int]
getPosibilities fila = [index | (b,index)<-zip booleans [1..9], b == True]
   where distinct n = map (/=n) (filter (/=0) fila)
         booleans   = map (all (==True))  (map distinct [1..9])
-- Each element which is zero, has a column and a row. In each of them, if there are numbers
-- the possibles selections could be the ones not in row and not in column
-- elements in that row
-- elements in that column.
-- [2,3,4] [1,3,5]  la única eleccion es aquella que está en ambas.
getCommons posFilas posCols = [p | p <- posCols, isIn p] 
     where isIn n = elem n posFilas
-- getPosibilities of an element which is zero
getRealPosibles index sodoku = getCommons posiblesRow  posiblesCol
    where cols_index  = getCindex index              -- column index of the element
          rows_index  = getRindex index              -- row index    of the element
          row         = getRows sodoku !! rows_index -- row
          col         = getCols sodoku !! cols_index -- col
          posiblesRow = getPosibilities row          -- posibles values looking at row
          posiblesCol = getPosibilities col          -- posibles values looking at col
