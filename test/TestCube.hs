module TestCube (testsCube) where

import Test.QuickCheck
import Cube
--import Test.QuickCheck.Gen
import Data.Group



testsCube :: IO ()
testsCube = do 
    putStrLn "\nStarting tests of Cube: \n\n"
    quickCheck ( asociativity)
    quickCheck ( neutral1)
    quickCheck ( neutral2)
    quickCheck ( inverse1)
    quickCheck ( inverse2)
    quickCheck (inversedoble)

     --add new properties to test


--define properties:

--Properties of a group (associativity, neutral element, inverse)

asociativity :: Cube -> Cube -> Cube -> Property
asociativity c1 c2 c3 = (c1 <> (c2 <> c3)) === ((c1 <> c2) <> c3)

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



instance Arbitrary Cube where
    arbitrary = do
        xs <- shuffle [0..53]
        return $ newCubeFromList xs
    --Random stickers permutations. Can be improved with real perms

