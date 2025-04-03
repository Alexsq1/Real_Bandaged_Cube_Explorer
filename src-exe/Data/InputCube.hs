module InputCube(cubeFromManimCodification, bandagedCubeScratchIO) where

import Data.List.Split
import qualified Data.Vector as V
import Cube
import InputBandagedCube
import Bandaged
import Visualizator
import Moves

--SCRAMBLE: U2 F2 L2

--decir qué capa es (insert B: (alias))

-- | Definitive IO for asking the user to generate a Bandaged Cube
bandagedCubeScratchIO :: IO BandagedCube
bandagedCubeScratchIO = do

    putStrLn "IMPORTANT: for a proper visualization, place the cube with the white face on top and green on front"

    equivs <- faceAliases

    cube <- simpleInputCube equivs
    xs <- inputBlock equivs

    manimRecomendedVisualizer cube (Algorithm [])

    return $ newBandagedCube cube xs

-- | An IO that guides the user to insert a cube
simpleInputCube :: [(String, String)]  -> IO Cube
simpleInputCube equivs = do
    uLayer <- oneFace "U" equivs
    rLayer <- oneFace "R" equivs
    fLayer <- oneFace "F" equivs
    dLayer <- oneFace "D" equivs
    lLayer <- oneFace "L" equivs
    bLayer <- oneFace "B" equivs
    return $ cubeFromManimCodification equivs (uLayer ++ rLayer ++ fLayer ++ dLayer ++ lLayer ++ bLayer)


--Generate a list of tuples, with face and color [("U", "White")]

--[("U", "White"), ("F", "Green"), ("R", "Red"), ("L", "Orange"), ("B", "Blue"), ("D", "Yellow")]


-- | An IO that guides the user to insert the face aliases
faceAliases :: IO[(String, String)]
faceAliases = do
    putStrLn "Insert alias for U face"
    uLayer <- getLine
    putStrLn "Insert alias for R face"
    rLayer <- getLine
    putStrLn "Insert alias for F face"
    fLayer <- getLine
    putStrLn "Insert alias for D face"
    dLayer <- getLine
    putStrLn "Insert alias for L face"
    lLayer <- getLine
    putStrLn "Insert alias for B face"
    bLayer <- getLine
    return [("U", uLayer), ("R", rLayer), ("F", fLayer), ("L", lLayer), ("B", bLayer), ("D", dLayer)]

-- | IO that asks for the stickers of 1 face (private)
oneFace :: String -> [(String, String)] -> IO [String]
oneFace face eq = do
    putStrLn ("Insert " ++ face ++ " (alias " ++ alias ++ ") face colours, separated by spaces")
    face <- getLine
    return (((filter (/= "")) . (splitOn " " )) face)
    where
        alias = takeEquiv eq face

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
        strAliasToInts :: [String] -> [Int]
        strAliasToInts str
            | length str == 1 = centerToInt str
            | length str == 2 = edgeToInts str
            | length str == 3 = cornerToInts str
            | otherwise = []

        convertBlockToInts :: String -> [Int]
        convertBlockToInts str
            | null str = []
            | otherwise = concat $ map strAliasToInts colours
            where
                pieces = ( (filter (/= "")) . (splitOn "+" ) . filter (/= ' ') ) str
                stickers = map ( (filter (/= "")) . (splitOn "-" ) ) pieces
                colours = map (coloursToInitials equiv) stickers





-- | Returns the cube given by an equivalence of colours and a list in manim's order
cubeFromManimCodification :: [(String, String)] -- ^ Equivalence, like [(\"U\", \"White\"), (\"F\", \"Green\"), (\"R\", \"Red\"), (\"L\", \"Orange\"), (\"B\", \"Blue\"), (\"D\", \"Yellow\")]
            -> [String]                         -- ^ List of colours, like [\"White\", \"Green\", ...]
            -> Cube

cubeFromManimCodification equivalence str = newCubeFromList perm
    where
        perm = (stringToNum . reorder . coloursToInitials equivalence) str

reorder :: [String] -> [String]
reorder xs = V.toList (V.backpermute (V.fromList xs) (V.fromList arrangement))
    where
        arrangement = [6,18,38,0,36,47,2,45,11,8,9,20,29,
                        26,15,35,17,51,33,53,42,27,44,24,3,37,1,46,5,10,7,19,21,41,
                        23,12,48,14,50,39,28,25,32,16,34,52,30,43,4,22,13,49,40,31]
                            
coloursToInitials :: [(String, String)] -> [String] -> [String]
coloursToInitials eq xs = map (\str -> takeEquiv eq str) xs

--Searches through the tuples and gives the equivalence
takeEquiv :: Eq a => [(a,a)] -> a -> a
takeEquiv [] n = n
takeEquiv ((x,y):xs) current
    | y == current = x
    | x == current = y
    | otherwise = takeEquiv xs current

stringToNum :: [String] -> [Int]
stringToNum xs = (mapBy 3 cornerToInts corners) ++ (mapBy 2 edgeToInts edges) ++ (mapBy 1 centerToInt centers)
    where
        corners = take 24 xs
        edges = (take 24 . drop 24) xs
        centers = drop 48 xs

mapBy :: Int -> ([a] -> [b]) -> [a] -> [b]
mapBy _ _ [] = []
mapBy n f xs = (f prefix) ++ (mapBy n f (drop n xs))
    where 
        prefix = take n xs

centerToInt :: [String] -> [Int]
centerToInt ["U"] = [48]
centerToInt ["F"] = [49]
centerToInt ["R"] = [50]
centerToInt ["B"] = [51]
centerToInt ["L"] = [52]
centerToInt ["D"] = [53]
centerToInt _ = [-1]

edgeToInts :: [String] -> [Int]
edgeToInts [] = []
edgeToInts [_] = []
edgeToInts (f1 : f2 : _)
    | (not . null) opt1 = opt1
    | (not . null) opt2 = opt2
    | otherwise = []

    where
        opt1 = tryToMatch f1 f2
        opt2 = reverse (tryToMatch f2 f1)

        --tryToMatch always returns ascending lists
        tryToMatch :: String -> String -> [Int]
        tryToMatch "U" "L" = [24, 25]
        tryToMatch "U" "B" = [26, 27]
        tryToMatch "U" "R" = [28, 29]
        tryToMatch "U" "F" = [30, 31]

        tryToMatch "F" "L" = [32, 33]
        tryToMatch "F" "R" = [34, 35]
        tryToMatch "B" "R" = [36, 37]
        tryToMatch "B" "L" = [38, 39]

        tryToMatch "D" "F" = [40, 41]
        tryToMatch "D" "R" = [42, 43]
        tryToMatch "D" "B" = [44, 45]
        tryToMatch "D" "L" = [46, 47]

        tryToMatch _ _ = []

cornerToInts :: [String] -> [Int]
cornerToInts [] = []
cornerToInts [_] = []
cornerToInts [_,_] = []
cornerToInts (f1 : f2 : f3 : _)
    | (not . null) opt1 = opt1
    | (not . null) opt2 = (opt2 !! 2) : (opt2 !! 0) : (opt2 !! 1) : []
    | (not . null) opt3 = (opt3 !! 1) : (opt3 !! 2) : (opt3 !! 0) : []
    | (not . null) opt4 = (opt4 !! 1) : (opt4 !! 0) : (opt4 !! 2) : []
    | (not . null) opt5 = (opt5 !! 0) : (opt5 !! 2) : (opt5 !! 1) : []
    | (not . null) opt6 = (opt6 !! 2) : (opt6 !! 1) : (opt6 !! 0) : []
    | otherwise = []
--cornerToInts _ = []
    where
        opt1 = tryToMatch f1 f2 f3
        opt2 = tryToMatch f2 f3 f1
        opt3 = tryToMatch f3 f1 f2

        opt4 = tryToMatch f1 f3 f2
        opt5 = tryToMatch f3 f2 f1
        opt6 = tryToMatch f2 f1 f3
        --tryToMatch always returns ascending lists
        tryToMatch :: String -> String -> String -> [Int]
        tryToMatch "U" "F" "L" = [0, 1, 2]
        tryToMatch "U" "L" "B" = [3, 4, 5]
        tryToMatch "U" "B" "R" = [6, 7, 8]
        tryToMatch "U" "R" "F" = [9, 10, 11]

        tryToMatch "D" "F" "R" = [12, 13, 14]
        tryToMatch "D" "R" "B" = [15, 16, 17]
        tryToMatch "D" "B" "L" = [18, 19, 20]
        tryToMatch "D" "L" "F" = [21, 22, 23]
        tryToMatch _ _ _ = []


