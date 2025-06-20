module TestCube (testsCube) where

import Test.QuickCheck
import Cube
--import Test.QuickCheck.Gen
import Data.Group



testsCube :: IO ()
testsCube = do
    quickCheck ( associativity)
    quickCheck ( neutral1)
    quickCheck ( neutral2)
    quickCheck ( inverse1)
    quickCheck ( inverse2)
    quickCheck (inversedoble)

     --add new properties to test


--define properties:

--Properties of a group (associativity, neutral element, inverse)

associativity :: Cube -> Cube -> Cube -> Property
associativity c1 c2 c3 = (c1 <> (c2 <> c3)) === ((c1 <> c2) <> c3)

neutral1 :: Cube -> Property
neutral1 c = (c <> mempty) === c

neutral2 :: Cube -> Property
neutral2 c = (mempty <> c) === c

inverse1 :: Cube -> Property
inverse1 c = (c <> (invert c)) === mempty

inverse2 :: Cube -> Property
inverse2 c = ((invert c) <> c) === mempty

inversedoble :: Cube -> Property
inversedoble c = ( (invert . invert) c) === c



{-
PROBAR
props matemáticas de grupos (orden), algoritmos que resuelven,
...
-}



