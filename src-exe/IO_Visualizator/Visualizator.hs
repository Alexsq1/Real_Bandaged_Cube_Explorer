module Visualizator(manimCustomVisualizer, manimRecomendedVisualizer) where

import Cube
import Moves
import ManimHsConversion(toManimCodification)
import System.Process (spawnProcess)

-- | Generates a video with manim, with recomended options for rendering
manimRecomendedVisualizer :: Cube -- ^ Initial position
    -> String                     -- ^ Color scheme ("" for default)
    -> Algorithm                  -- ^ Algorithm executed
    -> IO()

manimRecomendedVisualizer cube "" algorithm = do
    manimCustomVisualizer 10 1.5 "low_quality" cube "WHITE,#B90000,#009B48,#FFD500,#FF5900,#0045AD" algorithm
manimRecomendedVisualizer cube scheme algorithm = do
    manimCustomVisualizer 10 1.5 "low_quality" cube scheme algorithm

-- | Generates a video with manim, allowing configuration
manimCustomVisualizer :: Int -- ^ Seconds rotating over the diagonal
    -> Float                -- ^ Seconds for each move
    -> String               -- ^ "low_quality" or "high_quality"
    -> Cube                 -- ^ Initial position
    -> String               -- ^ String of colours in order URFLDB. Example "WHITE,#B90000,#009B48,#FFD500,#FF5900,#0045AD" 
    -> Algorithm            -- ^ Algorithm executed
    -> IO()

manimCustomVisualizer tRotation tMoves quality cube scheme alg = do
    _ <- spawnProcess "python"
        ["src-exe/IO_Visualizator/manim_cube_visualizator.py", 
        show tRotation, 
        show tMoves, 
        quality, 
        toManimCodification cube, 
        scheme,
        show alg]
    putStrLn "Generating video with Manim"

--Shell command that works:
--python manim_cube 
--Python3 and python3.8 fails
