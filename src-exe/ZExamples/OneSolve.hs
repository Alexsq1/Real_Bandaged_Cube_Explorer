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

import Data.Maybe
--import qualified Data.Set as S



oneSolve :: IO ()
oneSolve = do

    let c = newBandagedCube (newCubeFromList [0..53]) [[]]

    --let alg = read "R U F2 R' F' U F' U F R2 F' " :: Algorithm
    let alg = read "R U R' U R U R2 U R' U' R U2 R2 U R' U R U R U'" :: Algorithm
    
    
    let c1 = fromJust (tryToExecuteAlg c alg)
--    let solution1 = genericSearch c1 solvedBC [R, U] (noHeuristic)
    let solution1 = genericSearch c1 solvedBC [R, U] (noHeuristic)
    putStrLn ("Solution found: " ++ (show solution1))
    
    --6-gen: working for lenght <=7. Starting to have troubles in length 8
    --3-gen: working for lenght <=10. Starting to have troubles in length 8
    --2-GEN: finding 15-length solutions in < 10 seconds