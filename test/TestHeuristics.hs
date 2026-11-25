module TestHeuristics (testHeuristics) where

import Test.QuickCheck

import Moves
import Bandaged
import TestCube     -- imports arbitrary of Algorithm
import CubeCreator(newSolvedBandagedCube)
import Data.Maybe
import KorfHeuristic
import LoadKorfHeuristics(loadVectors, lookupAll)
import CalculateKorfHeuristics(stdVectors)

import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V

type V8 = V.Vector (Word8)
type HVector = (Word8, V8, V8, V8)



testHeuristics :: IO()
testHeuristics = do

        -- test of Korf heuristics (admissible)
        let depth = 8
        v <- (loadVectors depth)
        --quickCheck (sharing depth)
        --quickCheck (sharingCorners depth 1)
        --quickCheck (sharingEdges1 depth 1)
        --quickCheck (sharingEdges2 depth 1)
        quickCheck (admisibleCornerHeuristic v)
        quickCheck (admisibleEdgeFstHeuristic v)
        quickCheck (admisibleEdgeSndHeuristic v)
        quickCheck (korfAdmissible v)
        --quickCheck (properGenerationCorner depth v)

sharing :: Word8 -> Word8 -> Property
sharing d n = n <= d ==> ioProperty $
                do 
                    v0 <- loadVectors n 
                    v1 <- stdVectors n
                    return (v0 == v1)

sharingCorners :: Word8 -> Word8 -> Property
sharingCorners d n = n <= d ==> ioProperty $
                do 
                    (_,v0,_,_) <- loadVectors n 
                    (_,v1,_,_) <- stdVectors n
                    return (V.find (\(a,b) -> a /= b) (V.zip v0 v1) == Nothing )

sharingEdges1 :: Word8 -> Word8 -> Property
sharingEdges1 d n = n <= d ==> ioProperty $
                do 
                    (_,_,v0,_) <- loadVectors n 
                    (_,_,v1,_) <- stdVectors n
                    return (V.find (\(a,b) -> a /= b) (V.zip v0 v1) == Nothing )

sharingEdges2 :: Word8 -> Word8 -> Property
sharingEdges2 d n = n <= d ==> ioProperty $
                do 
                    (_,_,_,v0) <- loadVectors n 
                    (_,_,_,v1) <- stdVectors n
                    return (V.find (\(a,b) -> a /= b) (V.zip v0 v1) == Nothing )


admisibleCornerHeuristic :: HVector -> Algorithm -> Property
admisibleCornerHeuristic v alg =
    property((fromIntegral cornerH) <= (lengthAlg alg))
        where
            finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
            (cornerH, _, _) = lookupAll v (stdCube finalSt)

admisibleEdgeFstHeuristic :: HVector -> Algorithm -> Property
admisibleEdgeFstHeuristic v alg =
    property((fromIntegral edge1H) <= (lengthAlg alg))
        where
            finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
            (_, edge1H, _) = lookupAll v (stdCube finalSt)

admisibleEdgeSndHeuristic :: HVector -> Algorithm -> Property
admisibleEdgeSndHeuristic v alg =
    property((fromIntegral edge2H) <= (lengthAlg alg))
        where
            finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
            (_, _, edge2H) = lookupAll v (stdCube finalSt)

korfAdmissible :: HVector -> Algorithm -> Property
korfAdmissible v alg = property (h <= (lengthAlg alg))
    where
        finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
        h = korfHeuristic v finalSt

-- todo: adjusted heuristics: length > generated => h = generated + 1

--properGenerationCorner :: Word8 -> HVector -> Algorithm -> Property
--properGenerationCorner d v alg 
--    | la <= d = property (cornerH == la)
--    | otherwise = property (cornerH == d + 1)
--        where
--            la = fromIntegral (lengthAlg alg)
--            finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
--            (cornerH, _, _) = lookupAll v (stdCube finalSt)

-- interesting, but U2 D2 U2 is not simplified. TODO: fix it

