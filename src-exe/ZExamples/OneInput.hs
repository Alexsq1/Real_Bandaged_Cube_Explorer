module OneInput(oneInput) where


--when cabal repl, used modules must be imported here
--import Cube
--import Bandaged
import Moves
--import InputCube
--import InputBandagedCube
import Visualizator
import ManimHsConversion
--import Search
--import Heuristic

--import Data.Maybe
--import qualified Data.Set as S



oneInput :: IO ()
oneInput = do


    ----SCRAMBLE: U' F U2 B R2 L2 D' L' B2 R' B2 D2 L2 D F2 U R2 F2 U2 F2 D
--    let alg = read "U' F U2 B R2 L2 D' L' B2 R' B2 D2 L2 D F2 U R2 F2 U2 F2 D" :: Algorithm
    let inv = read "D' F2 U2 F2 R2 U' F2 D' L2 D2 B2 R B2 L D L2 R2 B' U2 F' U" :: Algorithm

    let eq = [("U", "white"), ("F", "green"), ("R", "red"), ("L", "orange"), ("B", "blue"), ("D", "yellow")]
    let state = ["red","white","white","white","white","green","yellow","yellow","white",
                "orange","orange","green","white","red","blue","white","yellow","orange",
                "red","orange","blue","yellow","green","orange","yellow","blue","red",
                "orange","white","blue","green","yellow","blue","green","red","blue",
                "yellow","green","blue","green","orange","red","white","yellow","green",
                "red","red","green","orange","blue","red","yellow","blue","orange"]

    let c = cubeFromManimCodification eq state
    manimRecomendedVisualizer c (inv)


{-
["red","white","white","white","white","green","yellow","yellow","white",
"orange","orange","green","white","red","blue","white","yellow","orange","red","orange","blue","yellow","green","orange","yellow","blue","red","orange","white","blue","green","yellow","blue","green","red","blue","yellow","green","blue","green","orange","red","white","yellow","green","red","red","green","orange","blue","red","yellow","blue","orange"]
-}