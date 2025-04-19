module Search(numberCanonicalSequences, genericSearch, idaStar, SearchingState(..), dfsSgle, dfsMult) where

import Bandaged
--import qualified Data.Set as S
import Moves
import Data.Maybe

-- | SearchingState storages all the information needed to to a Search.
data SearchingState = SearchingState {found :: Bool, 
                            initialState :: BandagedCube, 
                            currentDepth :: Int,
                            maximumDepth :: Int,
                            condition :: (BandagedCube -> Bool),
                            solution :: [Turn], 
                            validLayers :: [Face],
                            listMoves :: [Turn], 
                            lastFace :: Face,
                            heuristic :: (BandagedCube -> Int)}
                            
                            --to be added: min node that surpassed the bound

instance Show SearchingState where
    show (SearchingState f ini currD maxD _ sol layers _ lstFace _) = 
        "found: " ++ show f ++  "\n" ++
        "initial state: " ++ show ini ++  "\n" ++
        "current depth: " ++ show currD ++  "\n" ++
        "maximum depth: " ++ show maxD ++ "\n" ++
        "solution: " ++ show sol ++ "\n" ++
        "generation of moves: " ++ show layers ++
        "last face executed: " ++ show lstFace
        
        
-- | A list with all the possible 1-face moves
--allPossibleMoves :: [Turn]
--allPossibleMoves = [Turn(f, m) | f <- [R .. ] , m <- [1..3]]

numberCanonicalSequences :: Int -> Int
numberCanonicalSequences 0 = 1
numberCanonicalSequences 1 = 18
numberCanonicalSequences 2 = 18 * 3 * 4 + 18 * 3 `div` 2    --243
numberCanonicalSequences n = fibIterate 2 n 243 18
    where
        fibIterate :: Int -> Int -> Int -> Int -> Int
        fibIterate gen goal current prev
            | gen == goal = current
            | otherwise = fibIterate (gen + 1) goal (12 * current + 18 * prev) current


-- | Recieves data, makes a generic bounded search and compose the solution
genericSearch :: BandagedCube               -- ^ Initial state
                -> (BandagedCube -> Bool)   -- ^ Condition to determine a Node is found
                -> [Face]                   -- ^ List of Turns to generate a new node
                -> (BandagedCube -> Int)    -- ^ Heuristic (must be admissible)
                -> Maybe Algorithm          -- ^ The solution



genericSearch ini cond validLs h
    | found search = Just (Algorithm (solution search))     --Solution found
    | otherwise = Nothing                                   --Solution not found
    where

        nums = [1 .. 3]
        gen1 = [ Turn(f, n) | f <- validLs, n <- nums]
        --To be improved: use only free faces (delete non movable)

        initialSS = SearchingState{found = False, initialState = ini,
                                    currentDepth = 0, maximumDepth = 0, 
                                    condition = cond,
                                    solution = [], validLayers = validLs, 
                                    listMoves = gen1, 
                                    lastFace = N, 
                                    heuristic = h}
                                    
                                    --Start initial max depth with heuristic of initial node
        search = idaStar initialSS


idaStar :: SearchingState -> SearchingState
idaStar initSS
    | found thisSearchSS = thisSearchSS
    | (treshold <= 20) = idaStar (initSS {maximumDepth = treshold + 1})     --Update max depth with minimum node that exceeded the max.
    | otherwise = initSS
    where
        thisSearchSS = dfsSgle initSS
        treshold = maximumDepth initSS

-- | Search with dfs from one node
dfsSgle :: SearchingState                           -- ^ Initial Searching State
            -> SearchingState                       -- ^ Final Searching State

dfsSgle initialSS
    | pred ini = 
        initialSS {found = True}                                                --solution found       
    | currD >= maxD || (currD + h ini >= maxD) =                                --pruning, reached maximum depth
        initialSS
    | otherwise =                                                               --intermediate, keep searching
        dfsMult initialSS movesToIterate

    where
        (SearchingState _ ini currD maxD pred _ _ movesValid lstFace h) = initialSS
        movesToIterate = filter (predCanonicSequence lstFace) movesValid
        --This makes canonical sequences
        
        predCanonicSequence :: Face -> Turn -> Bool
        predCanonicSequence lsface (Turn(f,_)) = 
            (axisOfFace f /= axisOfFace lsface) || (f > lsface)

-- | Search with dfs algorithm. Iterate over several move generation
dfsMult :: SearchingState                       -- ^ Initial
            -> [Turn]                           -- ^ List of turns used to generate branches
            -> SearchingState                   -- ^ Final

dfsMult initialSS [] = initialSS                                    --ended iterating
dfsMult initialSS (x:xs)                                            --keep iterations
    | found thisBrach = thisBrach {solution = (x : solutionP)}      --Correct branch, recompose solution
    | currD > maxD = initialSS                                      --pruning (difficult with good heuristics)            
    | isNothing nextState = dfsMult initialSS xs                    --Not valid turn, breaks a block
    | otherwise = dfsMult initialSS xs                              --Incorrect branch, keep searching

    where
        (SearchingState _ ini currD maxD _ _ _ _ _ _) = initialSS

        nextState = tryToTurn ini x

        (Turn(lastFaceExecuted, _)) = x

        thisBrach = dfsSgle (initialSS
            {initialState = fromJust nextState, 
            currentDepth = currD + 1,
            lastFace = lastFaceExecuted} )

        solutionP = solution thisBrach
        
