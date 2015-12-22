-- La siguiente configuracion nos permite realizar test unitarios y test de propiedades de forma conjunta

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
 
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit

-- definicion de prueba
prueba :: Integer -> Integer
prueba n = n + 1

tests = [
        testGroup "Testing implementations for exercises" 
           [ testCase "prueba con 1" $ prueba 1 @?= 2
           , testCase "prueba con 2" $ prueba 2 @?= 3
           , testProperty "Prueba de propiedad sencilla" prop1
           ]  
        ]                


-- propiedad de prueba
prop1 n = prueba n == n + 1
   where types = (n::Integer)  
-- tests de prueba
test_1 = prueba 1 @?= 2
test_2 = prueba 1 @?= 2 
main :: IO ()
main = defaultMain tests                                                                                 