module OneSolve(oneSolve) where

import Bandaged
import Moves
import InputBandagedCube
import Heuristic
import Data.Maybe
import SolvingStrategies

oneSolve :: IO ()
oneSolve = do

    --GENERATING ALGS AND CHECKING 1 HEURISTIC

    --let alg = read "R U F2 R' F' U F' U F R2  " :: Algorithm
    --let alg = read "R U R' U R U R2 U R' U' R U' R2 U R' U R U R " :: Algorithm       --length 17
    --let alg = read " U R U R U2 R' " :: Algorithm
    --let alg = read "R U " :: Algorithm
    let alg = read "B2 U' L' D L U R D2 B2 " :: Algorithm      --length 16
    --let alg = read "R L' U2 L R' U2 R L' U2 R L " :: Algorithm      --checks that RL are in the right order
    --let alg = read "D R2 U F2 R F  " :: Algorithm      --6-GEN


    --L2, U, D Cube
    --let alg = read "D L2 U D' L U D L2 D U' L U D L " :: Algorithm      --6-GEN
    --let bcs = newBandagedCube (newCubeFromList [0..53]) [[49,50,51]]
    --let c1 = fromJust (tryToExecuteAlg bcs alg)
    --let h = {-# SCC "Heuristic_Generation" #-} korfIndivHeuristics c1
    --putStrLn ("Individual heuristics: " ++ show(h))
    --let solution1 = korfLayersSolver [L,U,D] c1
    --putStrLn ("Solution found: " ++ (show solution1))
    
    
    --SOLVING 1 CUBE
    let c1 = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
    let solution1 = korfSolver c1
    let h = {-# SCC "Heuristic_Generation" #-} korfIndivHeuristics c1
    putStrLn ("Individual heuristics: " ++ show(h))
    putStrLn ("Solution found: " ++ (show solution1))

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