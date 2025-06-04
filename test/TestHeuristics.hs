module TestHeuristics (testHeuristics) where

import Test.QuickCheck

import Moves
import Bandaged
import InputBandagedCube
import Data.Maybe
import Data.List
import KorfHeuristic
import Combinatorics
import IndexHeuristics


testHeuristics :: IO()
testHeuristics = do
        quickCheck (perfectHashingPerms)
        quickCheck (perfectHashingNPR)
--        quickCheck perfectHashingCP
--        quickCheck perfectHashingEP
--        quickCheck perfectHashingBC
        putStrLn "Generating pattern databases, be patient"
        quickCheck admisibleCornerHeuristic
        quickCheck admisibleEdgeFstHeuristic
        quickCheck admisibleEdgeSndHeuristic
        quickCheck korfAdmissible
        

perfectHashingPerms :: Property
perfectHashingPerms = property (sort numbering == [minimum numbering .. maximum numbering])
    where
        perms = permutations [0 .. 7]
        numbering = map (factorialNumbering) perms

perfectHashingNPR :: Property
perfectHashingNPR = property (sort numbering == [minimum numbering .. maximum numbering])
    where
        vars = variations 6 [0..11]
        numbering = map (nprNumbering [0..11]) vars

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
        cornerH = case (korfIndivHeuristics finalSt) of
            ([_, x, _]) -> x
            _ -> 1000

admisibleEdgeFstHeuristic :: Algorithm -> Property
admisibleEdgeFstHeuristic alg = property (edgeH <= length xs)
    where
        Algorithm xs = alg
        finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
        edgeH = case (korfIndivHeuristics finalSt) of
            ([_, x, _]) -> x
            _ -> 1000

admisibleEdgeSndHeuristic :: Algorithm -> Property
admisibleEdgeSndHeuristic alg = property (edgeH <= length xs)
    where
        Algorithm xs = alg
        finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
        edgeH = case (korfIndivHeuristics finalSt) of
            ([_, _, x]) -> x
            _ -> 1000
        
korfAdmissible :: Algorithm -> Property
korfAdmissible alg = property (h <= lengthAlg alg)
    where
        finalSt = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
        h = korfHeuristic finalSt

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
