module OneSolve(oneSolve) where


--when cabal repl, used modules must be imported here
import Cube
import Bandaged
import Moves
--import InputCube
import InputBandagedCube
--import Visualizator
--import ManimHsConversion
import Search
import Heuristic
import GenKorfHeuristics

import Data.Maybe
--import qualified Data.Set as S



oneSolve :: IO ()
oneSolve = do

    let c = newSolvedBandagedCube

    --let alg = read "R U F2 R' F' U F' U F R2 F' " :: Algorithm
--    let alg = read "R U R' U R U R2 U R' U' R U' R2 U R' U R U R " :: Algorithm       --length 17
    let alg = read " U R" :: Algorithm
    --let alg = read "R U R' U R U R2 U2 R' U' R U2 R2 U R' U R U R U'" :: Algorithm      --lenth 16
    --let alg = read "R U' L' U R' U' L U R U R' " :: Algorithm      --length 16
    --let alg = read "R L' U2 L R' U2 R L' U2 R L " :: Algorithm      --checks that RL are in the right order
        
    
    let c1 = fromJust (tryToExecuteAlg c alg)
    --let solution1 = genericSearch c1 solvedBC [R, U] (noHeuristic)
    let solution1 = genericSearch c1 solvedBC [R, U] (korfHeuristic)
    --let solution1 = genericSearch c1 solvedBC [R, U, F] (noHeuristic)
    --let solution1 = genericSearch c1 solvedBC [R,U,L] (noHeuristic)
    putStrLn ("Solution found: " ++ (show solution1))
    
    --WORKING: in < 10 seconds
    --6-gen: working for lenght <=7. Starting to have troubles in length 8
    --3 adj-gen: working for length <=10. Starting to have troubles in length 11
    --3 paralel-gen: working perfect for length <= 11. Starting to have troubles in length 12
    --2-GEN: finding 15-length solutions in < 10 seconds. 17-length in <30 seconds