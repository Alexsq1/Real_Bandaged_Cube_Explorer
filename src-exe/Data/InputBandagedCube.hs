module InputBandagedCube(newBandagedCube, manimCustomVisualizer, manimRecomendedVisualizer) where

import Moves
import Cube
import Bandaged
import qualified Data.Set as S
import qualified Data.Vector as V
import System.Process (spawnProcess)

-- | Creates a new Bandaged Cube, with a standard cube and the restrictions
newBandagedCube :: Cube -> [[Int]] -> BandagedCube
newBandagedCube cubeOrigin blocks = BandagedCube {stdCube = cubeOrigin, restrictions = postProcessedBlocks}
    where
        blocksSet = doubleListToDoubleSet blocks
        postProcessedBlocks = (expandBlocks . canonicSets) blocksSet


--All auxiliary from here

doubleListToDoubleSet :: Ord a => [[a]] -> S.Set (S.Set a)
doubleListToDoubleSet xs = S.fromList (map S.fromList xs)



-- | Join (union) the sets that are not disjoint.
canonicSets :: S.Set (S.Set Int) -> S.Set (S.Set Int)
canonicSets ss = genUnions (S.toList ss) (doubleListToDoubleSet [[]])
--Maybe it can be improved with folds, and not convert to lists back again

---- Auxiliar of canonic set. Takes a list of sets, makes the unions recursively
genUnions :: [S.Set Int] -> S.Set (S.Set Int) -> S.Set (S.Set Int)
genUnions [] xs = S.delete (S.empty) xs 
genUnions (s0:rest) temp = genUnions rest (S.insert unitedSet notCombining)
    where
        (notCombining, toComb) = S.partition (S.disjoint s0) temp
        unitedSet = S.unions (S.insert s0 toComb)
        --Generates the union of the non-disjoint sets

--Tries to expand all the blocks
expandBlocks :: S.Set (S.Set Int) -> S.Set (S.Set Int)
expandBlocks setOfBlocks = S.map expand1Block setOfBlocks

--Expand a single block, with all the possible turns
expand1Block :: S.Set Int -> S.Set Int
expand1Block block = pieceIntersections (expansions)
    where
        expansions = map (expandBlockTurn block) [R,U,L,D,B,F]
    
--Given a block and a Face, returns the expanded block
expandBlockTurn :: S.Set Int -> Face -> S.Set Int
expandBlockTurn block move
    | isBlockPreserved = rightSubSet
    | otherwise = allPieces
    where
        (xs1, xs2) = divideTurn move
        --Every turn divides the cube in 2 sets: pieces affected and not affected.
        --The block is preserved when the full block is in one of the 2 sub-partitions
        isBlockPreserved = (S.disjoint block xs1) || (S.disjoint block xs2)
        
        --There are going to be intersections. Must return the proper sub-partition
        rightSubSet = if (S.disjoint block xs2) then xs1 else xs2

--Keep in mind: it returns the pieces of the solved state. 
--Maybe it is better to do that in the scrambled state.
--Maybe it is better to preprocess the cube, and detect it in the checking state 


pieceIntersections :: (Foldable f) => f (S.Set Int) -> S.Set Int
pieceIntersections = foldl' S.intersection (full)
    where
        full = allPieces




----preprocesar bloques: piezas adyacentes entre sí.
----misma pieza en varios bloques: hacer unión.
----Piezas adyacentes: en un bloque, al menos uno lo rompería o impediría.
----Un bloque siempre tiene alguna arista. Puede incluir esquinas o centros (mínimo uno)


manimRecomendedVisualizer :: Cube -> Algorithm -> IO()
manimRecomendedVisualizer cube algorithm = do
    manimCustomVisualizer 8 1 "low_quality" cube algorithm
  

manimCustomVisualizer :: Int -> Float -> String -> Cube -> Algorithm -> IO()
manimCustomVisualizer tRotation tMoves quality cube alg = do
    _ <- spawnProcess "python"
        ["src-exe/Data/manim_cube_visualizator.py", 
        show tRotation, show tMoves, quality, 
        toManimCodification cube, 
        show alg]
    putStrLn "Generating video with Manim"

--Can be added: list of color scheme (with hexadecimal values of colors)


--manimTest :: IO()
--manimTest = do
--    spawnProcess "python" ["src-exe/Data/manim_cube_visualizator.py", "4", "2", "low_quality", "UUUUUUUUURLRRRRRRRLFFFFFFFFDDDDDDDDDFRBLLLLLLBBLBBBBBB", "L' U' L U L F' L2 U L U L' U' L F "]
--    putStrLn "End"

{-
-}

--1º arg: estado cubo -> backpermute cubo xsraro -> piece to face

--funciona python manim_cube... y python3.8. python3 no funciona
toManimCodification :: Cube -> String
toManimCodification (Cube xs) = foldl' (\acc face -> acc ++ (show face)) "" listFaces
    where
        permutation = [3,26,6,24,48,28,0,30,9, 10,29,8,35,50,37,14,43,16,
            1,31,11,32,49,34,23,41,13,21,40,12,46,53,42,18,44,15,
            4,25,2,39,52,33,20,47,22,7,27,5,36,51,38,17,45,19]
        
        reorder = V.toList (V.backpermute xs (V.fromList permutation))
        listFaces = map pieceToFace reorder


pieceToFace :: Int -> Face
pieceToFace n = xs !! n
    where 
        xs = [U,F,L,U,L,B,U,B,R,U,R,F,D,F,R,D,R,B,D,B,L,D,L,F,U,L,U,B,U,R,U,F,F,L,F,R,B,R,B,L,D,F,D,R,D,B,D,L,U,F,R,B,L,D]

--UFLULBUBRURFDFRDRBDBLDLFULUBURUFFLFRBRBLDFDRDBDLUFRBLD