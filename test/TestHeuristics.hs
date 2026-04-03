module TestHeuristics (testHeuristics) where

import Test.QuickCheck

import Moves
import Bandaged
--import InputBandagedCube
import CubeCreator(newSolvedBandagedCube)
import Data.Maybe
import Data.List
import KorfHeuristic
import LoadKorfHeuristics(loadVectors, lookupAll)
import Combinatorics
import IndexHeuristics

import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V

type V8 = V.Vector (Word8)
type HVector = (V8,V8,V8)


testHeuristics :: IO()
testHeuristics = do
        quickCheck (perfectHashingPerms)
        quickCheck (perfectHashingNPR)
        quickCheck perfectHashingCP
        quickCheck perfectHashingEP
        quickCheck perfectHashingBC
        quickCheck factInverse1
        quickCheck factInverse2
        quickCheck nprInverse1
        quickCheck nprInverse2
        let depth = 7
        v <- (loadVectors depth)
        quickCheck (admisibleCornerHeuristic v)
        quickCheck (admisibleEdgeFstHeuristic v)
        quickCheck (admisibleEdgeSndHeuristic v)
        quickCheck (korfAdmissible v)

perfectHashingPerms :: Property
perfectHashingPerms = property (sort numbering == [minimum numbering .. maximum numbering])
    where
        perms = permutations [0 .. 7]
        numbering = map (factorialEncode) perms

perfectHashingNPR :: Property
perfectHashingNPR = property (sort numbering == [minimum numbering .. maximum numbering])
    where
        vars = variations 6 [0..11]
        numbering = map (nprEncode) vars

        variations :: Int -> [Int] -> [[Int]]
        variations 0 _ = [[]]
        variations _ [] = [[]]
        variations k xs = [y:ys | (y,rest) <- select xs, ys <- variations (k-1) rest]
            where
                select [] = []
                select (x:xss) = (x,xss) : [(y,x:ys) | (y,ys) <- select xss]

perfectHashingCP :: Property
perfectHashingCP = forAll (genCPs 2000) $
                (\perms -> 
                    let indexs = map factorialEncode perms
                    in length (nub indexs) == length indexs)

genCPs :: Int -> Gen [[Int]]
genCPs n = go n []
    where
        go 0 acc = return acc
        go k acc = do
            p <- gen1CP
            if p `elem` acc 
                then go k acc
                else go (k-1) (p:acc)

gen1CP :: Gen [Int]
gen1CP = shuffle [0 .. 7]

perfectHashingEP :: Property
perfectHashingEP = forAll (genEPHalves 2000) $
                (\perms -> 
                    let indexs = map (nprEncode) perms
                    in length (nub indexs) == length indexs)

genEPHalves :: Int -> Gen [[Int]]
genEPHalves n = go n []
    where
        go 0 acc = return acc
        go k acc = do
            p <- gen1EP
            let p3 = take 6 p
            if p3 `elem` acc 
                then go k acc
                else go (k-1) (p3:acc)

gen1EP :: Gen [Int]
gen1EP = shuffle [0 .. 11]

perfectHashingBC :: [BandagedCube] -> Property
perfectHashingBC bcList = let hashes = map (\(BandagedCube c _) -> (cornersKey c, edgesKeyFst c, edgesKeySnd c)) bcList
                        in property (length (nub bcList) == length (nub hashes))

nprInverse1 :: Property
nprInverse1 = forAll gen1EP $ \x ->
    ((nprDecode) . (nprEncode)) (take 6 x) == take 6 x &&
    ((nprDecode) . (nprEncode)) (drop 6 x) == drop 6 x

nprInverse2 :: Int -> Property
nprInverse2 n = n >= 0 && n < 665273 ==>
    ((nprEncode) . (nprDecode)) n == n

factInverse1 :: Property
factInverse1 = forAll gen1CP $ \x ->
    ((factorialDecode) . (factorialEncode)) x == x

factInverse2 :: Int -> Property
factInverse2 n = n >= 0 && n < 40320 ==>
    (factorialEncode . (factorialDecode)) n == n

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
