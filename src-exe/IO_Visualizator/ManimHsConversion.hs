module ManimHsConversion(cubeFromManimCodification, toManimCodification, facePieceToInts, swapByEquivalent) where

import Cube
import Moves(Face(..))

-- | Given a cube, returns the cube in Manim codification (initial of face in its order)
toManimCodification :: Cube -> String
toManimCodification cube = foldl' (\acc face -> acc ++ (show face)) "" listFaces
    where
        permutation = [3,26,6,24,48,28,0,30,9, 10,29,8,35,50,37,14,43,16,
            1,31,11,32,49,34,23,41,13,21,40,12,46,53,42,18,44,15,
            4,25,2,39,52,33,20,47,22,7,27,5,36,51,38,17,45,19]
        
        xs = allPieces cube
        reorder = map (xs !! ) permutation
        listFaces = map pieceToFace reorder

pieceToFace :: Int -> Face
pieceToFace n = xs !! n
    where 
        xs = [U,F,L,U,L,B,U,B,R,U,R,F,D,F,R,D,R,B,D,B,L,D,L,F,U,L,U,B,U,R,U,F,F,L,F,R,B,R,B,L,D,F,D,R,D,B,D,L,U,F,R,B,L,D]

-- | Returns the cube given by an equivalence of colours and a list in manim's order
cubeFromManimCodification :: [(String, String)] -- ^ Equivalence, like [(\"U\", \"White\"), (\"F\", \"Green\"), (\"R\", \"Red\"), (\"L\", \"Orange\"), (\"B\", \"Blue\"), (\"D\", \"Yellow\")]
            -> [String]                         -- ^ List of colours, like [\"White\", \"Green\", ...]
            -> Cube

cubeFromManimCodification equivalence str = newCubeFromList perm
    where
        perm = (stringToNum . reorder . swapByEquivalent equivalence) str

        --Makes the permutation from manim codification to mine
        reorder :: [String] -> [String]
        reorder xs = map (xs !!) arrangement
            where
                arrangement = [6,18,38,0,36,47,2,45,11,8,9,20,29,
                                26,15,35,17,51,33,53,42,27,44,24,3,37,1,46,5,10,7,19,21,41,
                                23,12,48,14,50,39,28,25,32,16,34,52,30,43,4,22,13,49,40,31]

-- | Given tuples of equivalences, makes all the equivalences.
swapByEquivalent :: [(String, String)] -> [String] -> [String]
swapByEquivalent eq xs = map (takeEquiv eq) xs

-- | Given tuples of equivalences, returns the equivalent entry
takeEquiv :: Eq a => [(a,a)] -> a -> a
takeEquiv [] n = n
takeEquiv ((x,y):xs) current
    | y == current = x
    | x == current = y
    | otherwise = takeEquiv xs current

-- | given ["URF", "UR", "UL"] returns the ints values [0, 1, 2, 24, 25, 28, 29]
--(1 usage in InputCube inputBlock)
facePieceToInts :: [String] -> [Int]
facePieceToInts str
    | length str == 1 = centerToInt str
    | length str == 2 = edgeToInts str
    | length str == 3 = cornerToInts str
    | otherwise = []


stringToNum :: [String] -> [Int]
stringToNum xs = (mapBy 3 cornerToInts cornersP) ++ (mapBy 2 edgeToInts edgesP) ++ (mapBy 1 centerToInt centersP)
    where
        cornersP = take 24 xs
        edgesP = (take 24 . drop 24) xs
        centersP = drop 48 xs

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
