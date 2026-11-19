module TestCube (testsCube) where

import Test.QuickCheck
import Cube
import Bandaged
import Data.Set as S
import Data.Maybe(fromJust)
import Moves
import CubeCreator
import MathematicalNotation
import Data.Group
import TestMoves
--import Test.QuickCheck.Gen


testsCube :: IO ()
testsCube = do
    quickCheck ( associativity)
    quickCheck ( neutral1)
    quickCheck ( neutral2)
    quickCheck ( inverse1)
    quickCheck ( inverse2)
    quickCheck (inversedoble)
    quickCheck mathNotationCorrect1

     --add new properties to test

-- | GENERATOR OF CUBE: apply a random sequence of moves from the solved state

instance Arbitrary Cube where
    arbitrary = do
        alg <- arbitrary
        let cOrig = newSolvedCube
        let cLast = applyMove cOrig alg
        return cLast

-- GENERATOR OF BANDAGED CUBE: take an arbitrary cube with no blocks (useless)
instance Arbitrary BandagedCube where
    arbitrary = do
        alg <- arbitrary
        cube <- arbitrary
        let bcs = BandagedCube {stdCube = cube, 
            restrictions = S.singleton (S.empty)}
        return (fromJust (tryToExecuteAlg bcs alg))

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
 - to be tested:
 - mathematical properties of group theory:
 - order of a perm
-}


-- | Given a cube, calculates its mathematical notation and recomposes it
mathNotationCorrect1 :: Cube -> Property
mathNotationCorrect1 c = mathToCube (cp,co) (ep,eo) === c
    where
        (cp, co) = cornerState c
        (ep, eo) = edgesState c
-- the second version is in TestHeuristics, for the generators (todo: refactor)
