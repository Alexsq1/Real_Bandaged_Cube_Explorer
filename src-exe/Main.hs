module Main where


--when cabal repl, used modules must be imported here
import Cube
import Bandaged
import Moves
import InputCube
import InputBandagedCube
--import Visualizator
import ManimHsConversion
import Search
--import Heuristic

import Data.Maybe
--import qualified Data.Set as S

--This is the entry point of cabal run


{--

PROFILING:
PROFS:

cabal clean
cabal build --enable-profiling
cabal run exe:Bandaged-Cube-TFG -- +RTS -p

MEMORY (HEAP)

cabal run exe:Bandaged-Cube-TFG -- +RTS -hb
hp2ps -c Bandaged-Cube-TFG.hp

PHASES, time and memory for each

cabal run exe:Bandaged-Cube-TFG -- +RTS -s


CODE COVERAGE (check the flag -fhpc in config files)

cabal build --enable-profiling
cabal run
hpc report Bandaged-Cube-TFG
hpc markup Bandaged-Cube-TFG
(last 2, the name of the executable)
--}

main :: IO ()
main = do

    let c = newBandagedCube (newCubeFromList [0..53]) [[]]

    --let xs = [Turn(R,2), Turn(U,2), Turn(R, 2), Turn(U,2), Turn(R,2), Turn(U,3), Turn(R,2), Turn(U,1)]
    -- R2 U2 R2 U2 R2 U' R2 U

    --By the moment, working for lenght <=5. Starting to have troubles in length 6
    --let c1 = fromJust (tryToExecuteAlg c (Algorithm xs))
    --let solution1 = genericSearch c1 solvedBC allPossibleMoves (\_ -> 0)

    --putStrLn ("Solution found: " ++ (show solution1))
    
    let p = allPossibleMoves
    let xs = [[t1, t2, t3] | t1 <- p, t2 <- p, t3 <- p]
--    let xs = [[t1, t2] | t1 <- p, t2 <- p]
    let scr1Move = map (\alg -> fromJust (tryToExecuteAlg c (Algorithm alg))) xs
    let sols = map (\scr -> genericSearch scr solvedBC allPossibleMoves (\_ -> 0)) scr1Move
    let maxSol = maximum (map (\(Just (Algorithm xss)) -> length xss) sols)
    --putStrLn ("Solutions found: " ++ (show (zip xs sols)))
    putStrLn ("Maximum solution: " ++ (show maxSol))

    


    ----SCRAMBLE: U' F U2 B R2 L2 D' L' B2 R' B2 D2 L2 D F2 U R2 F2 U2 F2 D
    --let eq = [("U", "white"), ("F", "green"), ("R", "red"), ("L", "orange"), ("B", "blue"), ("D", "yellow")]
    --let state = ["red","white","white","white","white","green","yellow","yellow","white",
    --            "orange","orange","green","white","red","blue","white","yellow","orange",
    --            "red","orange","blue","yellow","green","orange","yellow","blue","red",
    --            "orange","white","blue","green","yellow","blue","green","red","blue",
    --            "yellow","green","blue","green","orange","red","white","yellow","green",
    --            "red","red","green","orange","blue","red","yellow","blue","orange"]
    --let c = cubeFromManimCodification eq state
    --manimRecomendedVisualizer c (Algorithm [])
--
{-
["red","white","white","white","white","green","yellow","yellow","white",
"orange","orange","green","white","red","blue","white","yellow","orange","red","orange","blue","yellow","green","orange","yellow","blue","red","orange","white","blue","green","yellow","blue","green","red","blue","yellow","green","blue","green","orange","red","white","yellow","green","red","red","green","orange","blue","red","yellow","blue","orange"]



-}