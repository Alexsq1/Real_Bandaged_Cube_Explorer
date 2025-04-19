module OneGen(oneGen) where


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


oneGen :: IO ()
oneGen = do

    let c = newBandagedCube (newCubeFromList [0..53]) [[]]

    --SEARCH ALL SOLUTIONS FOR ALL SCRAMBLES AT DEPTH 3:
    let p = [Turn(f, n) | f <- [R .. ], n <- [1 ..3]]
--    let xs = [[t1, t2] | t1 <- p, t2 <- p]
--    let xs = [[t1, t2, t3] | t1 <- p, t2 <- p, t3 <- p]
    let xs = [[t1, t2, t3, t4] | t1 <- p, t2 <- p, t3 <- p, t4 <- p]

    let scr1Move = map (\alg -> fromJust (tryToExecuteAlg c (Algorithm alg))) xs
--    let sols = map (\scr -> fromJust (genericSearch scr solvedBC [R .. ] (noHeuristic))) scr1Move
    let sols = map (\scr -> fromJust (genericSearch scr solvedBC [R .. ] (noHeuristic))) scr1Move
    putStrLn ("Finished depth , max sol: " ++ (show (maximum (map (\(Algorithm x) -> length x) sols))))
    

