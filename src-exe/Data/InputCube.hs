module InputCube(cubeFromManimCodification) where

--import Data.List.Split
import qualified Data.Vector as V
import Cube

--Generate a list of tuples, with face and color [("U", "White")]

--[("U", "White"), ("F", "Green"), ("R", "Red"), ("L", "Orange"), ("B", "Blue"), ("D", "Yellow")]

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

takeEquiv :: Eq a => [(a,a)] -> a -> a
takeEquiv [] n = n
takeEquiv ((x,y):xs) current
    | y == current = x
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
    | otherwise = []
--cornerToInts _ = []
    where
        opt1 = tryToMatch f1 f2 f3
        opt2 = tryToMatch f2 f3 f1
        opt3 = tryToMatch f3 f1 f2
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


--simpleInput :: IO [String]
--simpleInput = do
--    uLayer <- oneFace 'U'
--    rLayer <- oneFace 'R'
--    fLayer <- oneFace 'F'
--    dLayer <- oneFace 'D'
--    lLayer <- oneFace 'L'
--    bLayer <- oneFace 'B'
--    return (uLayer ++ rLayer ++ fLayer ++ dLayer ++ lLayer ++ bLayer)


--equivFaces :: IO [(String, String)]
--equivFaces = do
--    putStrLn "Insert U colour"
--    u <- getLine
--    putStrLn "Insert R colour"
--    r <- getLine
--    putStrLn "Insert F colour"
--    f <- getLine
--    putStrLn "Insert D colour"
--    d <- getLine
--    putStrLn "Insert L colour"
--    l <- getLine
--    putStrLn "Insert B colour"
--    b <- getLine 
--    return [("U", u), ("R", r), ("F", f), ("D", d), ("L", l), ("B", b)]

--oneFace :: Char -> IO [String]
--oneFace c = do
--    putStrLn ("Insert " ++ [c] ++ " face colours, separated by spaces")
--    face <- getLine
--    return (((filter (/= "")) . (splitOn " " )) face)