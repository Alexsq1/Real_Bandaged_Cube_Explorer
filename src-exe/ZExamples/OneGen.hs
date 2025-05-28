module OneGen(oneGen) where

import Bandaged
import Moves
import InputBandagedCube
import SolvingStrategies

import Data.Maybe

oneGen :: IO ()
oneGen = do

    let c = newSolvedBandagedCube

    --SEARCH ALL SOLUTIONS FOR ALL SCRAMBLES AT DEPTH 3:
    let p = [Turn(f, n) | f <- [R .. ], n <- [1 ..3]]
--    let xs = [[t1, t2] | t1 <- p, t2 <- p]
    let xs = [[t1, t2, t3] | t1 <- p, t2 <- p, t3 <- p]
--    let xs = [[t1, t2, t3, t4] | t1 <- p, t2 <- p, t3 <- p, t4 <- p]
--    let xs = [[t1, t2, t3, t4, t5] | t1 <- p, t2 <- p, t3 <- p, t4 <- p, t5 <- p]
--    let xs = [[t1, t2, t3, t4, t5, t6] | t1 <- p, t2 <- p, t3 <- p, t4 <- p, t5 <- p, t6 <- p]

    let scr1Move = map (\alg -> fromJust (tryToExecuteAlg c (Algorithm alg))) xs

    --let sols = map (\scr -> fromJust (korfSolver scr)) scr1Move
    let sols = map (\scr -> fromJust (iddfsSolver scr)) scr1Move
    
    putStrLn ("Finished depth , max sol: " ++ (show (maximum (map (\(Algorithm x) -> length x) sols))))
