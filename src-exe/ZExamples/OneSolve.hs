module OneSolve(oneSolve) where

import Bandaged
import Moves
import InputBandagedCube
import KorfHeuristic
import Data.Maybe
import SolvingStrategies
--import Cube

oneSolve :: IO ()
oneSolve = do

    --GENERATING ALGS AND CHECKING 1 HEURISTIC

    --let alg = read "R U F2 R' F' U F' U F R2  " :: Algorithm
    --let alg = read "R U R' U R U R2 U R' U' R U' R2 U R' U R U R " :: Algorithm       --length 17
    --let alg = read " U R U R U2 R' " :: Algorithm
    --let alg = read "R U " :: Algorithm
    --let alg = read "B2 U' L' D L U R D2 B2 " :: Algorithm      --length 16
    --let alg = read "R L' U2 L R' U2 R L' U2 R L " :: Algorithm      --checks that RL are in the right order
    --let alg = read "D R2 U F2 R F L D2 R' " :: Algorithm      --6-GEN
    --let bcs = newSolvedBandagedCube


    --L2, U, D Cube
    --let alg = read "D L2 U D' L U D L2 D U' L U D L " :: Algorithm
    --let bcs = newBandagedCube (newCubeFromList [0..53]) [[49,50,51]]
    --let c1 = fromJust (tryToExecuteAlg bcs alg)
    --let h = korfIndivHeuristics c1
    --putStrLn ("Individual heuristics: " ++ show(h))
    --let solution1 = smartKorfSolver c1
    --putStrLn ("Solution found: " ++ (show solution1))


    --Algorithm and blocks:

    ----TheMaoisha 252 
    let algMid = read "F' U' R' F2 R F U' R2 U R F' U R U2 R' F U F2 U' R' F' U' R' F' U2 F U R' F2 R F U' R F R2 F' U R U2 R' F' U' R' F' U' R2 U R F' U2 F U R' F U F2 U' R F R2 F' U' R' " :: Algorithm
    let alg = algMid <> algMid <> algMid <> algMid
    let blocks = [[51,52,53], [0,1,2,30,31], [21,22,23,40,41], [9,10,11,34,35,12,13,14], [6,7,8,28,29], [15,16,17,42,43]]    --Themaoisha 252
    
    ----Alcatraz
    --let blocks = [[3,48],[51,52,53],[32,23],[34,13],[16,37],[49,41],[50,43]]
    --let alg = read "U F' U' F2 R' F' R U F U' R' F' U' R U F R' F' R2 U' R' U " :: Algorithm

    --Bicube
    --let alg = read "F' U L F' L' F2 R2 U2 L' U R' U2 L U' F' U' F R U L' U2 R" :: Algorithm
    --let blocks = [[24,48],[52,47],[41,49],[37,50],[51,53],[3,26],[6,28],[0,30],[39,20],[32,23],[34,13],[43,16],[5,27]]
    
    --iddfs
    --let alg = read "R U R U R U' R2 U R U2 R'  " :: Algorithm
    --let blocks = [[49,52,53,51]]
    
    ----SOLVING 1 CUBE

    putStrLn ("Scramble of length " ++ (show (lengthAlg alg)) ++ ": " ++ (show alg))
    let bcSolved = newBandagedCube (newSolvedCube) blocks
    let c1 = fromJust (tryToExecuteAlg bcSolved alg)
    let solution1 = fromJust(smartKorfSolver c1)
    let h = korfIndivHeuristics c1
    putStrLn ("Individual heuristics: " ++ show(h))
    putStrLn ("Solution of length " ++ show (lengthAlg solution1) ++ " found: " ++ (show solution1))












    --OLD RESULTS

    --WORKING: in < 10 seconds
    --6-gen: working for lenght <=7. Starting to have troubles in length 8
    --3 adj-gen: working for length <=10. Starting to have troubles in length 11
    --3 paralel-gen: working perfect for length <= 11. Starting to have troubles in length 12
    --2-GEN: finding 15-length solutions in < 10 seconds. 17-length in <30 seconds