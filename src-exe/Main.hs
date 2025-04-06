module Main where


--when cabal repl, used modules must be imported here
import Cube
import Bandaged
import Moves
--import InputCube
import InputBandagedCube
--import Visualizator
import ManimHsConversion
import Search
--import Heuristic

import Data.Maybe
--import qualified Data.Set as S

--This is the entry point of cabal run
main :: IO ()
main = do

    let c = newBandagedCube (newCubeFromList [0..53]) [[]]
    let c1 = fromJust (tryToExecuteAlg c (Algorithm [Turn(R,1)]))

    let solution1 = genericSearch 1 c1 solvedBC allPossibleMoves (\_ -> 0)

    --let initialSS = SearchingState{found = False, initialState = c1, currentDepth = 0, maximumDepth = 1,  solution = [], searching = allPossibleMoves,  heuristic = (\_ -> 0), visitedStates = S.empty}
    --let search = dfsSgle initialSS solvedBC

    putStrLn ("Solution found: " ++ (show solution1))
    --putStrLn ("Detailed: \n" ++ (show search))




    --putStrLn "Testing a random state of a cube and displaying it"
--
    ----SCRAMBLE: U' F U2 B R2 L2 D' L' B2 R' B2 D2 L2 D F2 U R2 F2 U2 F2 D
--
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