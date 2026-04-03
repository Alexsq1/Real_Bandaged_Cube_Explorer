module HeuristicsProfile where

import Moves(Algorithm(..), Turn(..))
import Bandaged(tryToTurn, BandagedCube(..))
import IndexHeuristics(cornersKey, edgesKeySnd, edgesKeyFst)
--import LoadKorfHeuristics(loadVectors)
--import KorfHeuristic(korfIndivHeuristics)
import CubeCreator(newSolvedBandagedCube)
import Data.Maybe(fromJust)


prof :: IO (Int)
prof = do
    --h <- loadVectors n
    let Algorithm alg = read ("R U R2 D2 B' R U2 L2 B D F' L U U R L D2" ++
                    "F' U2 D2 F B R2 U' L' F R L2 B D R B' R' B' L U2 D2 R L B" ++
                    "F' U2 D2 F B R2 U' L' F R L2 B D R B' R' B' L U2 D2 R L B" ++
                    "F' U2 D2 F B R2 U' L' F R L2 B D R B' R' B' L U2 D2 R L B" ++
                    "F' U2 D2 F B R2 U' L' F R L2 B D R B' R' B' L U2 D2 R L B" ++
                    "F' U2 D2 F B R2 U' L' F R L2 B D R B' R' B' L U2 D2 R L B" ++
                    "F' U2 D2 F B R2 U' L' F R L2 B D R B' R' B' L U2 D2 R L B" ++
                    "F' U2 D2 F B R2 U' L' F R L2 B D R B' R' B' L U2 D2 R L B" ++
                    "F' U2 D2 F B R2 U' L' F R L2 B D R B' R' B' L U2 D2 R L B" ++
                    "B' F' U R U2 B2 F' D2 R U L D L F' U2 F R2 D L B2") :: Algorithm
    let m = sumPartialHeurs alg
    return m

sumPartialHeurs :: [Turn] -> Int
sumPartialHeurs alg = 
    fst $ foldl' body (0, newSolvedBandagedCube) alg
    where 
        --body = \(n, b) move -> (n + sum (korfIndivHeuristics h b), newB b move)
        body = \(n, b) move -> (n + sumKeys b, newB b move)
        newB = \bb m -> fromJust $ tryToTurn bb m

sumKeys :: BandagedCube -> Int
sumKeys b = cornersKey b + edgesKeyFst b + edgesKeySnd b
