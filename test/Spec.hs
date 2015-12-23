-- La siguiente configuracion nos permite realizar test unitarios y test de propiedades de forma conjunta

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
 
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit
import Lib          -- Functions

import Datatest     -- data for testing

-- definicion de prueba
prueba :: Integer -> Integer
prueba n = n + 1

tests = [
        testGroup "Testing implementations for exercises" 
           [ testCase "prueba con 1" $ prueba 1 @?= 2                -- some notation
           , testCase "prueba con 2" test_2
           , testCase "Solucion real"          test_isSolution_1     -- must be true
           , testCase "Se repiten una cifra"   test_isSolution_2     -- must be false
           , testCase "Tiene un cero"          test_isSolution_3     -- must be false
           , testCase "bd1 debe ser valido"    test_isValid_0        -- must be true
           , testCase "bd1 debe ser valido"    test_isValid_1
           , testCase "bd1 debe ser valido"    test_isValid_2
           , testCase "Counting well" $ length vacio @?= 81          -- verification 
           , testCase "First element" $ vacio !!  0 @?=  0           -- verification
           , testCase "one element" $ sol_bd1 !!  8 @?=  5           -- verification
           , testCase "one element" $ sol_bd1 !! 80 @?=  6           -- verification  
           , testCase "filas" $ getRindex  0 @?= 0                   -- must be zero 
           , testCase "filas" $ getRindex 80 @?= 8                   -- must be zero 
           , testCase "filas" $ getRindex 13 @?= 1                   -- must be zero 
           , testCase "columnas" $ getCindex  0 @?= 0                -- must be zero
           , testCase "columnas" $ getCindex 13 @?= 4                -- zero must be accounting 
           , testCase "obtener index" $ getIndex 1 4 @?= 13          -- zero must be accounting 
           , testCase "obtener index" $ getIndex 0 4 @?=  4          -- zero must be accounting
           , testCase "obtener elementos" test_getElement_0          -- zero must be accounting  
           , testCase "obtener elementos" test_getElement_1          -- zero must be accounting
           , testCase "obtener elementos" test_getElement_2          -- zero must be accounting 
           , testCase "obtener elementos" test_getCols_0             -- selecting columns of the structure             
           , testProperty "Reciprocidad" prop_testing_Index
           , testCase "areEquals" test_areEaquals_0
           , testCase "areEquals" test_areEaquals_1
           , testCase "areEquals" test_areEaquals_2 
           , testCase "indexes of cero elements 0" test_getZeroIndexes_0
           , testCase "indexes of cero elements 1" test_getPosibilities_0
           , testCase "set first Element" $ setElement 0 [1..5] 111 @?= [111,2,3,4,5]  -- index zero
           , testCase "set last Element"  $ setElement 4 [1..5] 111 @?= [1,2,3,4,111]  -- index less than 
           , testCase "set other Element" $ setElement 1 [1..5] 111 @?= [1,111,3,4,5]  -- second element         
           ]  
        ]                
-- Caso de prueba para ver si detecta las soluciones.
test_isSolution_1 = isSolution sol_bd1     @?=  True
test_isSolution_2 = isSolution no_sol_bd1  @?=  False 
test_isSolution_3 = isSolution no_sol2_bd1 @?= False
test_isValid_0    = isValid bd1            @?= True
test_isValid_1    = isValid sol_bd1        @?= True
test_isValid_2    = isValid no_sol_bd1     @?= False
-- Test for getting elements
test_getElement_0 = getElement sol_bd1 0 0 @?= 2
test_getElement_1 = getElement sol_bd1 8 8 @?= 6 
test_getElement_2 = getElement sol_bd1 1 4 @?= 2
-- Testing get columns of a soduku representation
test_getCols_0   = getCols sol_bd1   @?= columnas_sol_bd1
--Testing are equals
test_areEaquals_0 = areEquals [1..10]           @?= True
test_areEaquals_1 = areEquals [1,3,4,6,6,6,2,0] @?= False
test_areEaquals_2 = areEquals [1,3,3,4,6,6,2,0] @?= False 
--testing getZeroIndexes 
test_getZeroIndexes_0 = getZeroIndexes [1,2,0,0,9] @?= [2,3]
--testing posibilities
test_getPosibilities_0 = getPosibilities ((getRows bd1) !! 1) @?= [2,3,4,6,7,8] 
-- properties
prop_testing_Index n = 
     (n >= 0) ==>      -- valores no negativos
     getIndex (getRindex n) (getCindex n) == n
       where types = (n::Int)   






-- propiedad de prueba
prop1 n = prueba n == n + 1
   where types = (n::Integer)  
-- tests de prueba
test_1 = prueba 1 @?= 2
test_2 = prueba 1 @?= 2 
main :: IO ()
main = defaultMain tests


                                                                     