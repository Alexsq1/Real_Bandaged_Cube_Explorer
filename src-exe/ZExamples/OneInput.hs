module OneInput(oneInput) where

import ManimHsConversion
import SolvingStrategies
import InputBandagedCube
--import Heuristic
--import Moves
--import Visualizator
import Data.Maybe(fromJust)

oneInput :: IO ()
oneInput = do

    --TESTING WITH A 3X3, NON BANDAGED

--    ----SCRAMBLE: U' F U2 B R2 L2 D' L' B2 R' B2 D2 L2 D F2 U R2 F2 U2 F2 D
--    let alg = read "U' F U2 B R2 L2 D' L' B2 R' B2 D2 L2 D F2 U R2 F2 U2 F2 D" :: Algorithm
--    let inv = read "D' F2 U2 F2 R2 U' F2 D' L2 D2 B2 R B2 L D L2 R2 B' U2 F' U" :: Algorithm
----
    let eq = [("U", "w"), ("F", "g"), ("R", "r"), ("L", "o"), ("B", "b"), ("D", "y")]
--    let state = ["r","w","w","w","w","g","y","y","w",
--                "o","o","g","w","r","b","w","y","o",
--                "r","o","b","y","g","o","y","b","r",
--                "o","w","b","g","y","b","g","r","b",
--                "y","g","b","g","o","r","w","y","g",
--                "r","r","g","o","b","r","y","b","o"]
----
--    let c = cubeFromManimCodification eq state
--    manimRecomendedVisualizer c (inv)

    --let solution1 = korfSolver c
    --putStrLn ("Solution found by Korf solver: " ++ (show solution1))




    --TESTING WITH A BICUBE

    --SCRAMBLE: L' D F D' L B D L D' B2 L' B D L' D' L'

--    let state = ["w", "r", "r", "w", "w", "w", "r", "o", "o",
--                "g", "r", "w", "w", "r", "r", "r", "r", "w",
--                "b", "y", "y", "b", "g", "g", "y", "g", "g",
--                "b", "y", "y", "w", "y", "y", "w", "o", "o",
--                "b", "b", "y", "b", "o", "y", "b", "o", "o",
--                "g", "g", "r", "b", "b", "o", "g", "g", "o"]
--
--    let c = cubeFromManimCodification eq state
--
--    let bc = newBandagedCube c [[0,1,2,32,33],[3,4,5,38,39],[6,7,8,26,27],[9,10,11,34,35],[12,13,14,42,43],[15,16,17,44,45],[21,22,23,46,47],[24,25,52],[28,29,48,50],[30,31,49],[36,37,51],[40,41,53]]
--
--    let h = korfIndivHeuristics bc
--    putStrLn ("Individual heuristics: " ++ show(h))
--
--    let solution = smartKorfSolver bc
--    putStrLn ("Solution found by smart Korf solver: " ++ (show solution))
 

    --Other scrambles: D L2 B L' D' B' L' D2 F D' L2 F' L2 B D L D' B' D L

    --manimRecomendedVisualizer c (fromJust solution1)



    --TESTING WITH AN ALCATRAZ CUBE

--    --SCRAMBLE: R' U' R2 U R F R' U' R' U R' F' R U F U' F' U2
--
--    let state = ["w","w","r","w","w","w","o","w","w",
--                "o","o","w","g","r","g","g","r","g",
--                "w","b","b","b","g","o","b","g","o",
--                "y","y","y","y","y","y","y","y","y",
--                "b","r","g","o","o","r","o","o","r",
--                "g","g","r","r","b","b","r","b","b"]
--
--    let c = cubeFromManimCodification eq state
--
--    let bc = newBandagedCube c [[48,3],[51,52,53],[32,33],[49,41],[34,13],[50,43],[37,16]]
--
--    let solution = smartKorfSolver bc
----    let solution = korfSolver bc
--    putStrLn ("Solution found by smart Korf solver: " ++ (show solution))
--
--


    --TESTING WITH A FUSE CUBE (2-gen)


    let state = [   "r","w","r","b","w","g","w","w","w",
                    "g","r","b","r","r","b","w","o","b",
                    "r","r","o","g","g","y","g","g","b",
                    "y","y","o","y","y","w","y","y","r",
                    "y","r","g","o","o","o","o","o","o",
                    "w","g","g","w","b","b","y","b","b"]

    let c = cubeFromManimCodification eq state

    let bc = newBandagedCube c [[49,51,52,53]]

    let solution = smartKorfSolver bc
    putStrLn ("Solution found by smart Korf solver: " ++ (show solution))

