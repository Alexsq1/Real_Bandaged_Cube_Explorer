module OneSolve(oneSolve) where

import Bandaged
import Moves
import InputBandagedCube
import Heuristic
import Data.Maybe
import SolvingStrategies

oneSolve :: IO ()
oneSolve = do

    --let alg = read "R U F2 R' F' U F' U F R2  " :: Algorithm
    --let alg = read "R U R' U R U R2 U R' U' R U' R2 U R' U R U R " :: Algorithm       --length 17
    --let alg = read " U R U R U2 R' " :: Algorithm
    --let alg = read "R U " :: Algorithm
    --let alg = read "R U' L' U R' U' L U R U R' " :: Algorithm      --length 16
    --let alg = read "R L' U2 L R' U2 R L' U2 R L " :: Algorithm      --checks that RL are in the right order

    --let alg = read "D R2 U F2 R F  " :: Algorithm      --6-GEN
    let alg = read " U' D' R2" :: Algorithm      --6-GEN
    let c1 = fromJust (tryToExecuteAlg newSolvedBandagedCube alg)
    --let alg = read "D" :: Algorithm      --5-GEN

    let h = {-# SCC "Heuristic_Generation" #-} korfIndivHeuristics c1

    putStrLn ("Heuristics: " ++ show(h))

    --let solution1 = korfSolver c1
    --let solution1 = kociembaSolver c1

    --let solution1 = {-#Solution#-} genericSearch c1 solvedBC sixAxis (const 0)
    --let solution1 = {-#Solution#-} genericSearch c1 solvedBC sixAxis (korfHeuristic)
    --let solution1 = {-# SCC "Solution" #-} genericSearch c1 solvedBC (freeFaces [R, U, F, L, B]) (korfHeuristic)

    --putStrLn ("Solution found: " ++ (show solution1))
    
    --putStrLn ("Solution found by Kociemba: " ++ (show solution2))
    --putStrLn ("Solution found by Korf: " ++ (show solution1))
    
    --WORKING: in < 10 seconds
    --6-gen: working for lenght <=7. Starting to have troubles in length 8
    --3 adj-gen: working for length <=10. Starting to have troubles in length 11
    --3 paralel-gen: working perfect for length <= 11. Starting to have troubles in length 12
    --2-GEN: finding 15-length solutions in < 10 seconds. 17-length in <30 seconds