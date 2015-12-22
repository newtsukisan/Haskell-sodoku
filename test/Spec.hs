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
           , testProperty "Reciprocidad" prop_testing_Index
           ]  
        ]                
-- Caso de prueba para ver si detecta las soluciones.
test_isSolution_1 = isSolution sol_bd1 @?=     True
test_isSolution_2 = isSolution no_sol_bd1 @?=  False 
test_isSolution_3 = isSolution no_sol2_bd1 @?= False
test_getElement_0 = getElement sol_bd1 0 0 @?= 2
test_getElement_1 = getElement sol_bd1 8 8 @?= 6 
test_getElement_2 = getElement sol_bd1 1 4 @?= 2 

-- propiedades 
prop_testing_Index n = 
    not (n < 0) ==>      -- valores no negativos
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


                                                                     