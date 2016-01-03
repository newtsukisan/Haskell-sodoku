# Haskell-sodoku
Simple sodoku solver

The project use stack tool for all settings and testing enviroment. 

Here are some usefull types:

```haskell
type Soduku  = [Int]
type Row     = Int
type Col     = Int
type Index   = Int
type Rows    = [Int]
type Cols    = [Int]
```
Here the algorithm for solving the sodoku board.

```haskell
--                        SOLVING WITH A DEEP SEARCH
solve_Recursion :: Soduku -> Soduku
solve_Recursion sdk          = solveSingle sdk
-- Solve Single
solveSingle :: Soduku -> Soduku
solveSingle sdk                                   -- Solving in single case
     | isSolution sdk = sdk                       -- if is a solution ok
     | otherwise      = solveList (nextStep2 sdk) -- if not generate all posibles children

--Solve list of sodokus
solveList :: [Soduku] -> Soduku
solveList  listaSdk                               
     | listaSdk == [] = []                        -- Base Case
     | otherwise      = if not (try == []) then try else solveList (tail listaSdk) -- Search
     where  try   = solveSingle (head listaSdk)                                       
```
