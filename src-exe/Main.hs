module Main where

import Cube
import Moves
import InputCube
import Visualizator

--This is the entry point of cabal run
main :: IO ()
main = do
    putStrLn "Testing a random state of a cube and displaying it"

    --SCRAMBLE: U' F U2 B R2 L2 D' L' B2 R' B2 D2 L2 D F2 U R2 F2 U2 F2 D

    let eq = [("U", "white"), ("F", "green"), ("R", "red"), ("L", "orange"), ("B", "blue"), ("D", "yellow")]
    let state = ["red","white","white","white","white","green","yellow","yellow","white",
                "orange","orange","green","white","red","blue","white","yellow","orange",
                "red","orange","blue","yellow","green","orange","yellow","blue","red",
                "orange","white","blue","green","yellow","blue","green","red","blue",
                "yellow","green","blue","green","orange","red","white","yellow","green",
                "red","red","green","orange","blue","red","yellow","blue","orange"]
    let c = cubeFromManimCodification eq state
    manimRecomendedVisualizer c (Algorithm [])

{-
["red","white","white","white","white","green","yellow","yellow","white",
"orange","orange","green","white","red","blue","white","yellow","orange","red","orange","blue","yellow","green","orange","yellow","blue","red","orange","white","blue","green","yellow","blue","green","red","blue","yellow","green","blue","green","orange","red","white","yellow","green","red","red","green","orange","blue","red","yellow","blue","orange"]



-}