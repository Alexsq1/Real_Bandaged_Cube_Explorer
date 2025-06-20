module InputCube(bandagedCubeScratchIO) where

import Cube
import Bandaged
import InputBandagedCube
--import Visualizator
--import Moves
import ManimHsConversion

import Data.List.Split(splitOn)
import Data.List(intercalate)

-- | Definitive IO for asking the user to generate a Bandaged Cube
bandagedCubeScratchIO :: IO (BandagedCube, String)
bandagedCubeScratchIO = do

    --Input of colours
    equivs <- faceAliases
    sch <- colourScheme equivs

    --Input of the cube
    cube <- input54Stickers equivs

    --Input of the blocks
    xs <- inputBlock equivs

    return $ (newBandagedCube cube xs, sch)


--Example of list of tuples: [("U", "White"), ("F", "Green"), ("R", "Red"), ("L", "Orange"), ("B", "Blue"), ("D", "Yellow")]
-- | An IO that guides the user to insert the face aliases
faceAliases :: IO[(String, String)]
faceAliases = do
    putStrLn "Insert alias for U face (default: w)"
    uLayer <- getLine
    putStrLn "Insert alias for R face (default: r)"
    rLayer <- getLine
    putStrLn "Insert alias for F face (default: g)"
    fLayer <- getLine
    putStrLn "Insert alias for D face (default: y)"
    dLayer <- getLine
    putStrLn "Insert alias for L face (default: g)"
    lLayer <- getLine
    putStrLn "Insert alias for B face (default: b)"
    bLayer <- getLine

    let defaultInput = [("U", "w"), ("R", "r"), ("F", "g"), ("L", "o"), ("B", "b"), ("D", "y")]
    let input = [("U", uLayer), ("R", rLayer), ("F", fLayer), ("L", lLayer), ("B", bLayer), ("D", dLayer)]
    let sol = zipWith (\(i, alias) (i2,def) -> if(length alias == 0) then (i2, def) else (i,alias)) input defaultInput

    return (sol)

-- | An IO that guides the user to insert a cube
input54Stickers :: [(String, String)]  -> IO Cube
input54Stickers equivs = do
    uLayer <- oneFace "U" equivs
    rLayer <- oneFace "R" equivs
    fLayer <- oneFace "F" equivs
    dLayer <- oneFace "D" equivs
    lLayer <- oneFace "L" equivs
    bLayer <- oneFace "B" equivs
    return $ cubeFromManimCodification equivs (uLayer ++ rLayer ++ fLayer ++ dLayer ++ lLayer ++ bLayer)

-- | IO that asks for the stickers of 1 face (private)
oneFace :: String -> [(String, String)] -> IO [String]  
oneFace face eq = do
    putStrLn ("Insert " ++ face ++ " (alias " ++ alias ++ 
        ") face colours, separated by spaces (default: " ++ defaultFace ++ ")" )
    stickers <- getLine
    
    let provSol = ((filter (/= "")) . (splitOn " " )) (checkEmpty stickers)

    if ((canBeValid provSol) && (provSol !! 4) == alias) 
        then return (provSol)
        else (
            do
                putStrLn "Invalid input"
                strNext <- oneFace face eq
                return (strNext)
        )

    where
        result = swapByEquivalent eq [face]
        alias = case result of
                    [x] -> x
                    _   -> error "Unexpected result length"
        defaultFace = (concat . replicate 9) (alias ++ " ")

        checkEmpty :: String -> String
        checkEmpty "" = defaultFace
        checkEmpty str = str

        canBeValid :: [String] -> Bool
        canBeValid str = (length str == 9) && (all (\x -> x `elem` (map snd eq)) str)

--"WHITE,#B90000,#009B48,#FFD500,#FF5900,#0045AD"
--"#FFFFFF, RED,  GREEN,    YELLOW, ORANGE, BLUE"
colourScheme :: [(String, String)] -> IO String
colourScheme equiv = do
    putStrLn "\nDefault hexadecimal colours: "
    putStrLn "White: #FFFFFF"
    putStrLn "Red: #B90000"
    putStrLn "Green: #009B48"
    putStrLn "Yellow: #FFD500"
    putStrLn "Orange: #FF5900"
    putStrLn "Blue: #0045AD"
    putStrLn ""

    putStrLn ("Insert colour for U face (alias "++ (xsAlias !! 0) ++ ") (hexadecimal or by name, default #FFFFFF (white))")
    u0x <- getLine
    putStrLn ("Insert colour for R face (alias "++ (xsAlias !! 1) ++ ") (hexadecimal or by name, default #B90000 (red))")
    r0x <- getLine
    putStrLn ("Insert colour for F face (alias "++ (xsAlias !! 2) ++ ") (hexadecimal or by name, default #009B48 (green))")
    f0x <- getLine
    putStrLn ("Insert colour for D face (alias "++ (xsAlias !! 3) ++ ") (hexadecimal or by name, default #FFD500 (yellow))")
    d0x <- getLine
    putStrLn ("Insert colour for L face (alias "++ (xsAlias !! 4) ++ ") (hexadecimal or by name, default #FF5900 (orange))")
    l0x <- getLine
    putStrLn ("Insert colour for B face (alias "++ (xsAlias !! 5) ++ ") (hexadecimal or by name, default #0045AD (blue))")
    b0x <- getLine
    let xs = map checkEmpty [('U', u0x), ('R', r0x), ('F', f0x), ('D', d0x), ('L', l0x), ('B', b0x)]
    return $ intercalate "," xs
    
    where
        xsAlias = swapByEquivalent equiv ["U", "R", "F", "D", "L", "B"]

        checkEmpty :: (Char, String) -> String
        checkEmpty (face, "") = defaultColour face
        checkEmpty (_, ('#':colour)) = ('#':colour)
        checkEmpty (_, colour) = ('#':colour)

        defaultColour :: Char -> String
        defaultColour 'U' = "#FFFFFF"
        defaultColour 'R' = "#B90000"
        defaultColour 'F' = "#009B48"
        defaultColour 'D' = "#FFD500"
        defaultColour 'L' = "#FF5900"
        defaultColour 'B' = "#0045AD"
        defaultColour _ = "#555555" --dark grey

-- | An IO that guides the user to insert a block
inputBlock :: [(String, String)] -> IO [[Int]]
inputBlock equiv = do
    putStrLn "(Optional) insert a block in the format: c1-c2-c3+c1-c2+c1 (empty for finish)"
    str <- getLine

    --Keep on asking for input until null
    if (null str)
        then (return [[]])
        else do
            rest <- inputBlock equiv
            return ([convertBlockToInts str] ++ rest)
    where
        convertBlockToInts :: String -> [Int]
        convertBlockToInts str
            | null str = []
            | otherwise = concat $ map facePieceToInts colours
            where
                pieces = ( (filter (/= "")) . (splitOn "+" ) . filter (/= ' ') ) str
                stickers = map ( (filter (/= "")) . (splitOn "-" ) ) pieces
                colours = map (swapByEquivalent equiv) stickers
