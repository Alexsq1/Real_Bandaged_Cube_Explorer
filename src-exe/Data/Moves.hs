module Moves (Turn(..), Algorithm(..), Face(..), Axis(..), algToPerm, possibleTurns, permOfTurn, axisOfFace) where

import Data.Group
import Cube

import Test.QuickCheck
--import Test.QuickCheck.Gen


-- | A face of a Cube (R, U, F, L, D, B)
data Face = N | R | U | F | L | D | B deriving(Show, Eq, Ord, Read, Enum)

-- | A Turn is a face-move in OBTM (tuple of Face and Int)
newtype Turn = Turn(Face, Int) deriving (Eq)

instance Show Turn where
    show (Turn(face, degrees))
        | degrees == 2 = (show face) ++ "2"
        | degrees == 3 = (show face) ++ "'"
        | otherwise = (show face)

instance Read Turn where
    readsPrec _ (x:y:rest) = 
        let move = read [x] :: Face
            num =  read [y] :: Int
        in [ ( simpOneTurn (Turn(move, num)) , rest) ]

instance Read Algorithm where
    readsPrec _ str = [(  (staticReadAlg (canonic str)), [])]
    

staticReadAlg :: String -> Algorithm
staticReadAlg "" = Algorithm[]
staticReadAlg (x:y:xs) = Algorithm[read ([x] ++ [y])] <> staticReadAlg xs
staticReadAlg _ = Algorithm []


-- Transforms R U R' F2 into R1U1R3F2 (easier to read of turn to parse)
canonic :: String -> String
canonic str = insertOnes strPrimes
    where
        strNoSpaces = filter (/= ' ') str
        strPrimes = map (\x -> if (x == '\'') then '3' else x) strNoSpaces

insertOnes :: String -> String
insertOnes "" = ""
insertOnes (x:y:xs) = if ((isCharMove x && isCharMove y)) then ([x] ++ "1" ++ insertOnes(y:xs)) else ([x] ++ insertOnes (y:xs))
insertOnes (x:xs) = if (isCharMove x) then ([x] ++ "1" ++ insertOnes xs) else ([x] ++ insertOnes xs)

isCharMove :: Char -> Bool
isCharMove str = elem str (show [N .. ])

-- | Makes a list with all the possible turns
possibleTurns :: [Turn]
possibleTurns = [Turn(turn, degrees) | turn <- [R ..], degrees <- [1..3]]


-- | An Algorithm is a list of moves
data Algorithm = Algorithm [Turn] deriving (Eq)
--Improve Eq when they make the same permutation

instance Show Algorithm where
    show (Algorithm xs) = concat (map (\x -> (show x) ++ " ") xs)

instance Semigroup Algorithm where
    (Algorithm xs1) <> (Algorithm xs2) = Algorithm (simplifyTurns (xs1 ++ xs2))

simplifyTurns :: [Turn] -> [Turn]
simplifyTurns xs
    | (length simplifiedVersion) < (length xs) = simplifyTurns simplifiedVersion
    | otherwise = simplifiedVersion
    where 
        canonList = map simpOneTurn xs
        simplifiedVersion = simpAdjacentTurns canonList

simpAdjacentTurns :: [Turn] -> [Turn]
simpAdjacentTurns = foldl' myFunctionConcat []
    where 
        myFunctionConcat = 
            (\xs a -> 
                if (null xs)
                    then [a]
                else
                    ((init xs) ++ (simpTwoTurns (last xs) a))
            )


simpTwoTurns :: Turn -> Turn -> [Turn]
simpTwoTurns (Turn(face1, deg1)) (Turn(face2, deg2))
    | face1 == face2 && cancel = []
    | face1 == face2 = [simpOneTurn $ Turn(face1, sumD)]
    --add when same axis
    | otherwise = [Turn(face1, deg1), Turn(face2, deg2)]

    where 
        sumD = deg1 + deg2
        cancel = sumD `mod` 4 == 0

simpOneTurn :: Turn -> Turn
simpOneTurn (Turn(f, degree))
    | m == 0 = Turn(N, 0)
    | otherwise = Turn(f, m)
    where
        m = degree `mod` 4

instance Monoid Algorithm where
    mempty = Algorithm []

instance Group Algorithm where
    invert (Algorithm (xs)) = Algorithm(invertListofTurns xs)

invertTurn :: Turn -> Turn
invertTurn (Turn(f, n)) = Turn (f, (4-n))

invertListofTurns :: [Turn] -> [Turn]
invertListofTurns [] = []
invertListofTurns (x : xs) = invertListofTurns xs ++ [invertTurn x]


-- | Calculates the permutation an algorithm executes
algToPerm :: Algorithm -> Cube
algToPerm (Algorithm xs) = mconcat (map permOfTurn xs)

-- | Calculates the permutation a move executes
permOfTurn :: Turn -> Cube
permOfTurn (Turn(N, _)) = newCubeFromList [0..53]
permOfTurn (Turn(R, 1)) = newCubeFromList [0,1,2,3,4,5,11,9,10,13,14,12,17,15,16,7,8,6,18,19,20,21,22,23,24,25,26,27,34,35,30,31,32,33,42,43,28,29,38,39,40,41,36,37,44,45,46,47,48,49,50,51,52,53]
permOfTurn (Turn(R, 2)) = newCubeFromList [0,1,2,3,4,5,12,13,14,15,16,17,6,7,8,9,10,11,18,19,20,21,22,23,24,25,26,27,42,43,30,31,32,33,36,37,34,35,38,39,40,41,28,29,44,45,46,47,48,49,50,51,52,53]
permOfTurn (Turn(R, 3)) = newCubeFromList [0,1,2,3,4,5,17,15,16,7,8,6,11,9,10,13,14,12,18,19,20,21,22,23,24,25,26,27,36,37,30,31,32,33,28,29,42,43,38,39,40,41,34,35,44,45,46,47,48,49,50,51,52,53]
permOfTurn (Turn(U, 1)) = newCubeFromList [9,10,11,0,1,2,3,4,5,6,7,8,12,13,14,15,16,17,18,19,20,21,22,23,30,31,24,25,26,27,28,29,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53]
permOfTurn (Turn(U, 2)) = newCubeFromList [6,7,8,9,10,11,0,1,2,3,4,5,12,13,14,15,16,17,18,19,20,21,22,23,28,29,30,31,24,25,26,27,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53]
permOfTurn (Turn(U, 3)) = newCubeFromList [3,4,5,6,7,8,9,10,11,0,1,2,12,13,14,15,16,17,18,19,20,21,22,23,26,27,28,29,30,31,24,25,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53]
permOfTurn (Turn(F, 1)) = newCubeFromList [22,23,21,3,4,5,6,7,8,2,0,1,10,11,9,15,16,17,18,19,20,14,12,13,24,25,26,27,28,29,33,32,41,40,31,30,36,37,38,39,35,34,42,43,44,45,46,47,48,49,50,51,52,53]
permOfTurn (Turn(F, 2)) = newCubeFromList [12,13,14,3,4,5,6,7,8,21,22,23,0,1,2,15,16,17,18,19,20,9,10,11,24,25,26,27,28,29,40,41,34,35,32,33,36,37,38,39,30,31,42,43,44,45,46,47,48,49,50,51,52,53]
permOfTurn (Turn(F, 3)) = newCubeFromList [10,11,9,3,4,5,6,7,8,14,12,13,22,23,21,15,16,17,18,19,20,2,0,1,24,25,26,27,28,29,35,34,31,30,41,40,36,37,38,39,33,32,42,43,44,45,46,47,48,49,50,51,52,53]
permOfTurn (Turn(L, 1)) = newCubeFromList [5,3,4,19,20,18,6,7,8,9,10,11,12,13,14,15,16,17,23,21,22,1,2,0,38,39,26,27,28,29,30,31,24,25,34,35,36,37,46,47,40,41,42,43,44,45,32,33,48,49,50,51,52,53]
permOfTurn (Turn(L, 2)) = newCubeFromList [18,19,20,21,22,23,6,7,8,9,10,11,12,13,14,15,16,17,0,1,2,3,4,5,46,47,26,27,28,29,30,31,38,39,34,35,36,37,32,33,40,41,42,43,44,45,24,25,48,49,50,51,52,53]
permOfTurn (Turn(L, 3)) = newCubeFromList [23,21,22,1,2,0,6,7,8,9,10,11,12,13,14,15,16,17,5,3,4,19,20,18,32,33,26,27,28,29,30,31,46,47,34,35,36,37,24,25,40,41,42,43,44,45,38,39,48,49,50,51,52,53]
permOfTurn (Turn(D, 1)) = newCubeFromList [0,1,2,3,4,5,6,7,8,9,10,11,21,22,23,12,13,14,15,16,17,18,19,20,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,46,47,40,41,42,43,44,45,48,49,50,51,52,53]
permOfTurn (Turn(D, 2)) = newCubeFromList [0,1,2,3,4,5,6,7,8,9,10,11,18,19,20,21,22,23,12,13,14,15,16,17,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,44,45,46,47,40,41,42,43,48,49,50,51,52,53]
permOfTurn (Turn(D, 3)) = newCubeFromList [0,1,2,3,4,5,6,7,8,9,10,11,15,16,17,18,19,20,21,22,23,12,13,14,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,42,43,44,45,46,47,40,41,48,49,50,51,52,53]
permOfTurn (Turn(B, 1)) = newCubeFromList [0,1,2,8,6,7,16,17,15,9,10,11,12,13,14,20,18,19,4,5,3,21,22,23,24,25,37,36,28,29,30,31,32,33,34,35,45,44,27,26,40,41,42,43,39,38,46,47,48,49,50,51,52,53]
permOfTurn (Turn(B, 2)) = newCubeFromList [0,1,2,15,16,17,18,19,20,9,10,11,12,13,14,3,4,5,6,7,8,21,22,23,24,25,44,45,28,29,30,31,32,33,34,35,38,39,36,37,40,41,42,43,26,27,46,47,48,49,50,51,52,53]
permOfTurn (Turn(B, 3)) = newCubeFromList [0,1,2,20,18,19,4,5,3,9,10,11,12,13,14,8,6,7,16,17,15,21,22,23,24,25,39,38,28,29,30,31,32,33,34,35,27,26,45,44,40,41,42,43,37,36,46,47,48,49,50,51,52,53]
permOfTurn (Turn(t, x)) = permOfTurn(Turn(t, x `mod` 4))


--execTurn :: Cube -> Turn -> Cube
--execTurn (Cube v) t = Cube (newPerm)
--    where
--        newPerm = cyclePieces v (cycleOfTurn t)
--
-- | Cycle the pieces of a vector. Input cycles list, like [[0,1,2], [3,4]]
--cyclePieces :: V.Vector Int -> [[Int]] -> V.Vector Int
--cyclePieces v cycs = (V.//) v swaps
--    where
--        swaps = concat (map (listToSwaps v) cycs)
--
--listToSwaps :: V.Vector Int -> [Int] -> [(Int, Int)]
--listToSwaps orig cycs = map (\(a, b) -> (a, orig V.! b)) t
--    where
--        t = zip cycs (last cycs : init cycs)


--cycleOfTurn :: Turn -> [[Int]]
--cycleOfTurn (Turn(R,1)) = [[11,6,17,12], [9,7,15,13], [10,8,16,14], [28,36,42,34],[35,29,37,43]]
--cycleOfTurn (Turn(R,2)) = [[11,17], [6,12], [9,15], [7,13], [10,16], [8,14], [28,42],[36,34],[35,37],[29,43]]
--cycleOfTurn (Turn(R,3)) = [[12,17,6,11],[13,15,7,9],[14,16,8,10],[34,42,36,28],[43,37,29,35]]
--
--cycleOfTurn (Turn(U,1)) = [[0,3,6,9], [1,4,7,10], [2,5,8,11], [24,26,28,30],[25,27,29,31]]
--cycleOfTurn (Turn(U,2)) = [[0,6],[3,9],[1,7],[4,10],[2,8],[5,11],[24,28],[26,30],[25,29],[27,31]]
--cycleOfTurn (Turn(U,3)) = [[9,6,3,0],[10,7,4,1],[11,8,5,2],[30,28,26,24],[31,29,27,25]]
--
--cycleOfTurn (Turn(F,1)) = [[0,10,12,22], [1,11,13,23], [2,9,14,21], [31,34,41,32],[30,35,40,33]]
--cycleOfTurn (Turn(F,2)) = [[0,12],[10,22],[1,13],[11,23],[2,14],[9,21],[31,41],[34,32],[30,40],[35,33]]
--cycleOfTurn (Turn(F,3)) = [[22,12,10,0],[23,13,11,1],[21,14,9,2],[32,41,34,31],[33,40,35,30]]
--
--cycleOfTurn (Turn(L,1)) = [[2,22,20,4], [1,21,19,3], [0,23,18,5], [25,33,47,39],[24,32,46,38]]
--cycleOfTurn (Turn(L,2)) = [[2,20],[22,4],[1,19],[21,3],[0,18],[23,5],[25,47],[33,39],[24,46],[32,38]]
--cycleOfTurn (Turn(L,3)) = [[4,20,22,2],[3,19,21,1],[5,18,23,0],[39,47,33,25],[38,46,32,24]]
--
--cycleOfTurn (Turn(D,1)) = [[12, 15, 18, 21], [13, 16, 19, 22], [14,17,20,23], [40,42,44,46],[41,43,45,47]]
--cycleOfTurn (Turn(D,2)) = [[12,18],[15,21],[13,19],[16,22],[14,20],[17,23],[40,44],[42,46],[41,45],[43,47]]
--cycleOfTurn (Turn(D,3)) = [[21,18,15,12],[22,19,16,13],[23,20,17,14],[46,44,42,40],[47,45,43,41]]
--
--cycleOfTurn (Turn(B,1)) = [[3,20,15,8], [5,19,17,7], [4,18,16,6], [27,38,45,36],[26,39,44,37]]
--cycleOfTurn (Turn(B,2)) = [[3,15],[20,8],[5,17],[19,7],[4,16],[18,6],[27,45],[38,36],[26,44],[39,37]]
--cycleOfTurn (Turn(B,3)) = [[8,15,20,3],[7,17,19,5],[6,16,18,4],[36,45,38,27],[37,44,39,26]]
--
--cycleOfTurn (Turn(N, _)) = [[]]
--cycleOfTurn (Turn(t, x)) = cycleOfTurn(Turn(t, x `mod` 4))
--

-- | Axis of the cube
data Axis = RL | UD | FB | NN deriving(Show, Eq, Ord, Read, Enum)

-- | Returns the axis of a Face
axisOfFace :: Face -> Axis
axisOfFace R = RL
axisOfFace L = RL
axisOfFace U = UD
axisOfFace D = UD
axisOfFace F = FB
axisOfFace B = FB
axisOfFace N = NN

instance Arbitrary Turn where
    arbitrary = elements possibleTurns

instance Arbitrary Algorithm where
    arbitrary = do
        xs <- listOf arbitrary
        return (Algorithm xs <> mempty)
        --The mempty forces to simplify