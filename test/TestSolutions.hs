module TestSolutions (testSolutions) where

import Test.QuickCheck

import Cube
import Moves
import Bandaged
import CubeCreator
import Data.Maybe
import SolvingStrategies(smartKorfSolver)
import MoveGeneration(freeFaces)
import LoadKorfHeuristics(loadVectors)

import Debug.Trace
import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V

type V8 = V.Vector (Word8)
type HVector = (V8,V8,V8)

testSolutions :: IO()
testSolutions = do
    putStrLn "TESTS OF SEARCH ENGINE"

    let depth = 7
    v <- (loadVectors depth)
    
    --putStrLn "Testing a 3x3x3"
    --quickCheck (threeByThree v 8)

    --putStrLn "Testing a 2-gen"
    --quickCheck (twoGen v 25)

    --putStrLn "Testing a 3-gen RUF"
    --quickCheck (threeGenAdj v 14) --max 21

    --putStrLn "Testing a 3-gen RUL"
    --quickCheck (threeGenParalel v 14) --max 24

    --putStrLn "Testing a 4-gen RUFL"
    --quickCheck (fourGen v 12) --max ?

    --putStrLn "Testing a quad313"
    --quickCheck (quad313 v 30)

    --putStrLn "Testing a BiCube"
    --quickCheck (biCube v 30)

    --putStrLn "Testing an alcatraz"
    --quickCheck (alcatraz v 200)

    putStrLn "Testing a TheMaoiSha-252"
    quickCheck (theMaoisha252 v 300)

threeByThree :: HVector -> Int -> Property
threeByThree v n = korfSearchSolvesOptimally v generator [[]]
    where
        faces = [R .. ]
        generator = canonicAlgGenerator faces n

twoGen :: HVector -> Int -> Property
twoGen v n = korfSearchSolvesOptimally v generator [[49,51,52,53]]
    where
        faces = [R, U]
        generator = canonicAlgGenerator faces n

threeGenAdj :: HVector -> Int -> Property
threeGenAdj v n = korfSearchSolvesOptimally v generator [[51,52,53]]
    where
        faces = [R,U,F]
        generator = canonicAlgGenerator faces n

threeGenParalel :: HVector -> Int -> Property
threeGenParalel v n = korfSearchSolvesOptimally v generator [[49,51,53]]
    where
        faces = [R,U,L]
        generator = canonicAlgGenerator faces n

fourGen :: HVector -> Int -> Property
fourGen v n = korfSearchSolvesOptimally v generator [[51,53]]
    where
        faces = [R,U,F,L]
        generator = canonicAlgGenerator faces n

quad313 :: HVector -> Int -> Property
quad313 v n = korfSearchSolvesOptimally v generator blocks
    where
        faces = [R,U,F,L,D,B]
        blocks = [[0,21],[9,12],[6,15],[3,18]]
        generator = genWithLookAhead (newBandagedCube newSolvedCube blocks) faces n 
            

biCube :: HVector -> Int -> Property
biCube v n = korfSearchSolvesOptimally v generator blocks
    where
        faces = [R,U,F,L]
        blocks = [[24,48],[52,47],[41,49],[37,50],[51,53],
                [3,26],[6,28],[0,30],[39,20],[32,23],[34,13],
                [43,16],[5,27]]

        generator = subAlgsGenerator algs

        algs = map (\t -> read t :: Algorithm) 
            ["F' U L F' L' F2 R2 U2 L' U R' U2 L U' F' U' F R U L' U2 R",
            "U F2 R F2 L' U' R U' L U2 R' U' R' F R U F' L'",
            "F' U L F' L' F2 R2 U2 L' U R' U2 L U' F' U' F R'"]


alcatraz :: HVector -> Int -> Property
alcatraz v n = korfSearchSolvesOptimally v generator blocks
    where
        faces = [R,U,F]
        blocks = [[3,48],[51,52,53],[32,23],[34,13],[16,37],[49,41],[50,43]]
        --generator = genWithLookAhead (newBandagedCube newSolvedCube blocks) faces n 
        generator = subAlgsGenerator algs

        algs = map (\t -> read t :: Algorithm) 
            ["F U F' U2 R' U F2 R' F' R U F2 U' F R U F' U' F2 R2",
            "U' R U R2 F R F' U' R' U F R U F' U' R' F R'",
            "U' R U R' U F' U' F' R2 F2 R' U' R' U F' U F U'",
            "U' R U R2 F R F' U' R' U F R U F' U' R' F R F2 U F U' ",
            "U F' U' F2 R' F' R U F U' R' F' U' R U F R' F' R2 U' R' U "]   --22 moves

theMaoisha252 :: HVector -> Int -> Property
theMaoisha252 v n = korfSearchSolvesOptimally v generator blocks
    where
        faces = [R,U,F]
        blocks = [[51,52,53], [0,1,2,30,31], [21,22,23,40,41], [9,10,11,34,35,12,13,14], [6,7,8,28,29], [15,16,17,42,43]]
        alg = read "F' U' R' F2 R F U' R2 U R F' U R U2 R' F U F2 U' R' F' U' R' F' U2 F U R' F2 R F U' R F R2 F' U R U2 R' F' U' R' F' U' R2 U R F' U2 F U R' F U F2 U' R F R2 F' U' R' " :: Algorithm
    
--        generator = genWithLookAhead (newBandagedCube newSolvedCube blocks) faces n 
        generator = subAlgsGenerator [alg, alg <> alg, alg <> alg <> alg, alg <> alg <> alg <> alg]

korfSearchSolvesOptimally :: HVector -> Gen Algorithm -> [[Int]] -> Property
korfSearchSolvesOptimally v customGenerator blocks =
    let 
        origin = newBandagedCube (newSolvedCube) blocks
    in
    forAll (customGenerator) 
        $ \scramble ->
            let
                scrambeledCube = fromMaybe origin (tryToExecuteAlg origin scramble)
                solve = fromJust (smartKorfSolver v scrambeledCube)
                Algorithm xs1 = scramble
                Algorithm xs2 = solve
            in

            trace ("\n\nScramble: " ++ (show scramble) ++ "\nSolution: " ++ (show solve)) $
            collect (lengthAlg solve) $
    
            (solved (algToPerm (Algorithm (xs1 ++ xs2)))
             && (lengthAlg solve <= lengthAlg scramble)
            )

-----------------------------------------------------------------GENERATORS-------------------------------


possibleCanonicMovesPreserving :: BandagedCube -> [Face] -> Turn -> [Turn]
possibleCanonicMovesPreserving bcOrigin movableFaces lstTurn = filter (\(Turn(f,_)) -> validTurn bcOrigin f) (possibleCanonicMoves movableFaces lstTurn)

possibleCanonicMoves :: [Face] -> Turn -> [Turn]
possibleCanonicMoves movableFaces (Turn(lstFace, _)) = freeFaces possibleFaces
    where
        possibleFaces = [f | f <- movableFaces, 
                        (axisOfFace f /= axisOfFace lstFace) || (f > lstFace)]

-- | Generates an algorithm of a given maximum length with look ahead of 1 turn (avoiding non-exit branches)
genWithLookAhead :: BandagedCube -> [Face] -> Int -> Gen Algorithm
genWithLookAhead origin layers n = do
  k <- chooseInt (0, n)
  xs <- genListMovesLA origin k layers (Turn(N,0))
  return (Algorithm (xs) <> mempty)
  where

    genListMovesLA :: BandagedCube -> Int -> [Face] -> Turn -> Gen [Turn]
    genListMovesLA _ 0 _ _ = do
        return ( [Turn(N,0)] )
    genListMovesLA bCube n movable lst = do
        let toSelech = possiblesMovesLookAhead bCube movable lst
        if (null toSelech) 
            then return [Turn(N,0)]
        else do
            m <- elements toSelech
            let nextBC = fromJust (tryToTurn bCube m)
            rest <- genListMovesLA (nextBC) (n-1) movable m
            return (m: rest)

    possiblesMovesLookAhead :: BandagedCube -> [Face] -> Turn -> [Turn]
    possiblesMovesLookAhead bcOrigin movableFaces lstTurn = 
        map fst nextGenWithLookahead
        where
            possibleMoves = possibleCanonicMovesPreserving bcOrigin movableFaces lstTurn

            possibleNextGen = map (\move -> (move, tryToTurn bcOrigin move)) possibleMoves
            nextGen = ((map (\(m,bc) -> (m, fromJust bc))) . (filter (\(_, bc) -> isJust bc))) possibleNextGen

            nextGenWithLookahead = filter (\(_,bc) -> lookAhead bc movableFaces) nextGen

    lookAhead :: BandagedCube -> [Face] -> Bool
    lookAhead bCube movableFaces = any (\f -> validTurn bCube f) movableFaces

-- | Generates a canonic algorithm of a given maximum length (without checking the breaking of blocks)
canonicAlgGenerator :: [Face] -> Int -> Gen Algorithm
canonicAlgGenerator faces n = do
  k <- chooseInt (0, n)
  xs <- genListMoves k faces (Turn(N,0))
  return (Algorithm (xs))

genListMoves :: Int -> [Face] -> Turn -> Gen [Turn]
genListMoves 0 _ _ = do
    return []
genListMoves n movable lst = do
    m <- elements (possibleCanonicMoves movable lst)
    rest <- genListMoves (n-1) movable m
    return (m: rest)

-- | Generates a subalg of any algorithm of a list. Ugly but powerful
subAlgsGenerator :: [Algorithm] -> Gen Algorithm
subAlgsGenerator listAlgs = do
    (Algorithm moves) <- elements listAlgs
    k <- chooseInt (0, (length moves) - 1)
    return (Algorithm (take k moves))
