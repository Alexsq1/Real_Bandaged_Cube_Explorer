module TestHeuristics (testHeuristics) where

import Test.QuickCheck

import Moves
import Bandaged
import InputBandagedCube
import Data.Maybe
import Data.List
import Heuristic
import Combinatorics
import IndexHeuristics


testHeuristics :: IO()
testHeuristics = do
        quickCheck perfectHashingCP
        quickCheck perfectHashingEP
        quickCheck perfectHashingBC
        putStrLn "Generating pattern databases, be patient"
        quickCheck admisibleCornerHeuristic
        quickCheck admisibleEdgeHeuristic
        quickCheck korfAdmissible
        

gen1CP :: Gen [Int]
gen1CP = shuffle [0 .. 7]

genCPs :: Int -> Gen [[Int]]
genCPs n = go n []
    where
        go 0 acc = return acc
        go k acc = do
            p <- gen1CP
            if p `elem` acc 
                then go k acc
                else go (k-1) (p:acc)

gen1EP :: Gen [Int]
gen1EP = shuffle [0 .. 11]

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

perfectHashingCP :: Property
perfectHashingCP = forAll (genCPs 2000) $
                (\perms -> 
                    let indexs = map factorialNumbering perms
                    in length (nub indexs) == length indexs)

perfectHashingEP :: Property
perfectHashingEP = forAll (genEPHalves 2000) $
                (\perms -> 
                    let indexs = map (nprNumbering [0 .. 11]) perms
                    in length (nub indexs) == length indexs)

perfectHashingBC :: [BandagedCube] -> Property
perfectHashingBC bcList = let hashes = map (\c -> (cornersKey c, edgesKeyFst c, edgesKeySnd c)) bcList
                        in property (length (nub bcList) == length (nub hashes))


admisibleCornerHeuristic :: Algorithm -> Property
admisibleCornerHeuristic alg = property(cornerH <= length xs)
    where
        Algorithm xs = alg
        finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
        cornerH : _ = korfIndivHeuristics finalSt

admisibleEdgeHeuristic :: Algorithm -> Property
admisibleEdgeHeuristic alg = property (edgeH <= length xs)
    where
        Algorithm xs = alg
        finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
        [_, edgeH, _] = korfIndivHeuristics finalSt

korfAdmissible :: Algorithm -> Property
korfAdmissible alg = property (h <= length xs)
    where
        Algorithm xs = alg
        finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
        h = korfHeuristic finalSt
