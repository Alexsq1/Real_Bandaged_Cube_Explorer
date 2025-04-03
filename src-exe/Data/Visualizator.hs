module Visualizator(manimCustomVisualizer, manimRecomendedVisualizer) where

import Moves
import Cube
import qualified Data.Vector as V
import System.Process (spawnProcess)

-- | Generates a video with manim, with recomended options for rendering

manimRecomendedVisualizer :: Cube -- ^ Initial position
    -> Algorithm                  -- ^ Algorithm executed
    -> IO()

manimRecomendedVisualizer cube algorithm = do
    manimCustomVisualizer 10 1 "low_quality" cube algorithm

-- | Generates a video with manim, allowing configuration

manimCustomVisualizer :: Int -- ^ Seconds rotating over the diagonal
    -> Float                -- ^ Seconds for each move
    -> String               -- ^ "low_quality" or "high_quality"
    -> Cube                 -- ^ Initial position
    -> Algorithm            -- ^ Algorithm executed
    -> IO()

manimCustomVisualizer tRotation tMoves quality cube alg = do
    _ <- spawnProcess "python"
        ["src-exe/Data/manim_cube_visualizator.py", 
        show tRotation, show tMoves, quality, 
        toManimCodification cube, 
        show alg]
    putStrLn "Generating video with Manim"


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
