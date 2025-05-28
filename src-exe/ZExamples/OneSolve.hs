module OneSolve(oneSolve) where

import Bandaged
import Moves
import InputBandagedCube
import KorfHeuristic
import Data.Maybe
import SolvingStrategies
import Cube

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


    --alg for TheMaoisha 252 
    let alg = read "F' U' R' F2 R F U' R2 U R F' U R U2 R' F U F2 U' R' F' U' R' F' U2 F U R' F2 R F U' R F R2 F' U R U2 R' F' U' R' F' U' R2 U R F' U2 F U R' F U F2 U' R F R2 F' U' R' " :: Algorithm
    let Algorithm moves = alg
    
    ----SOLVING 1 CUBE
    putStrLn ("Alg: " ++ (show alg) ++ " of length " ++ (show (length moves)))
    let bcs = newBandagedCube (newCubeFromList [0..53]) [[51,52,53], [0,1,2,30,31], [21,22,23,40,41], [9,10,11,34,35,12,13,14], [6,7,8,28,29], [15,16,17,42,43]]    --Themaoisha 252
    let c1 = fromJust (tryToExecuteAlg bcs (alg <> alg <> alg <> alg))
    let solution1 = smartKorfSolver c1
    let Algorithm mSol = (fromJust solution1)
    let h = korfIndivHeuristics c1
    putStrLn ("Individual heuristics: " ++ show(h))
    putStrLn ("Solution found: " ++ (show solution1) ++ "\n of length " ++ show (length mSol))

    --let solution2 = kociembaSolver c1
    --let solution2 = {-#Solution#-} genericSearch c1 solvedBC sixAxis (const 0)
    --let solution2 = {-#Solution#-} genericSearch c1 solvedBC sixAxis (korfHeuristic)
    --let solution2 = {-# SCC "Solution" #-} genericSearch c1 solvedBC (freeFaces [R, U, F, L, B]) (korfHeuristic)

    --putStrLn ("Solution found by Kociemba: " ++ (show solution2))







    --CHECKING WHICH MOVES OF DEPTH D FAILS AT ITS HEURISTIC

    --let m = [ [Turn(f1,m1), Turn(f2, m2)] | f1 <- [N .. ], f2 <- [N .. ], m1 <- [1..3], m2 <- [1..3]]
    --let algs =  (map (Algorithm) (filter isCanonicalSecuence m))
    --let cubes = map (\alg -> (alg , fromJust(tryToExecuteAlg newSolvedBandagedCube alg))) algs
    --let heurs = map (\(a, c) -> (a, edgesKeySnd c, korfEdges2 c)) cubes
    ----let heursClean = nubBy (\(_, k1, _) (_, k2, _) -> k1 == k2) heurs
    ----let heursWrong = filter (\x -> snd x > 2) heurs
    --putStrLn ("Total heurs: " ++ show heurs)


    --RESULTS

    --WORKING: in < 10 seconds
    --6-gen: working for lenght <=7. Starting to have troubles in length 8
    --3 adj-gen: working for length <=10. Starting to have troubles in length 11
    --3 paralel-gen: working perfect for length <= 11. Starting to have troubles in length 12
    --2-GEN: finding 15-length solutions in < 10 seconds. 17-length in <30 seconds