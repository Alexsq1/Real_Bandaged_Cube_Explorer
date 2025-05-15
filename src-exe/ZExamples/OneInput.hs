module OneInput(oneInput) where

import Moves
import Visualizator
import ManimHsConversion
import Heuristic
import SolvingStrategies
import InputBandagedCube
import Data.Maybe(fromJust)

oneInput :: IO ()
oneInput = do


    ----SCRAMBLE: U' F U2 B R2 L2 D' L' B2 R' B2 D2 L2 D F2 U R2 F2 U2 F2 D
--    let alg = read "U' F U2 B R2 L2 D' L' B2 R' B2 D2 L2 D F2 U R2 F2 U2 F2 D" :: Algorithm
--    let inv = read "D' F2 U2 F2 R2 U' F2 D' L2 D2 B2 R B2 L D L2 R2 B' U2 F' U" :: Algorithm
--
--    let eq = [("U", "white"), ("F", "green"), ("R", "red"), ("L", "orange"), ("B", "blue"), ("D", "yellow")]
--    let state = ["red","white","white","white","white","green","yellow","yellow","white",
--                "orange","orange","green","white","red","blue","white","yellow","orange",
--                "red","orange","blue","yellow","green","orange","yellow","blue","red",
--                "orange","white","blue","green","yellow","blue","green","red","blue",
--                "yellow","green","blue","green","orange","red","white","yellow","green",
--                "red","red","green","orange","blue","red","yellow","blue","orange"]
--
--    let c = cubeFromManimCodification eq state
--    manimRecomendedVisualizer c (inv)

    --Scramble: D L2 B L' D' B' L' D2 F D' L2 F' L2 B D L D' B' D L

    let eq = [("U", "w"), ("F", "g"), ("R", "r"), ("L", "o"), ("B", "b"), ("D", "y")]
    let state = ["w", "r", "r", "w", "w", "w", "r", "o", "o",
                "g", "r", "w", "w", "r", "r", "r", "r", "w",
                "b", "y", "y", "b", "g", "g", "y", "g", "g",
                "b", "y", "y", "w", "y", "y", "w", "o", "o",
                "b", "b", "y", "b", "o", "y", "b", "o", "o",
                "g", "g", "r", "b", "b", "o", "g", "g", "o"]

    let c = cubeFromManimCodification eq state

    let bc = newBandagedCube c [[0,1,2,32,33],[3,4,5,38,39],[6,7,8,26,27],[9,10,11,34,35],[12,13,14,42,43],[15,16,17,44,45],[21,22,23,46,47],[24,25,52],[28,29,48,50],[30,31,49],[36,37,51],[40,41,53]]

    let h = {-# SCC "Heuristic_Generation" #-} korfIndivHeuristics bc
    putStrLn ("Individual heuristics: " ++ show(h))


    let solution1 = korfLayersSolver [F, B, L, D] bc
    putStrLn ("Solution found: " ++ (show solution1))
 

    manimRecomendedVisualizer c (fromJust solution1)
