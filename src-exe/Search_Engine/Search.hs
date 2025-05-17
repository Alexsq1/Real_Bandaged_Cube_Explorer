module Search(genericSearch) where

import Bandaged
import Moves
import Data.Maybe(fromJust, isNothing)
import Data.List(nub)

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
                            heuristic :: (BandagedCube -> Int),
                            minimumExceding :: Int      --Maybe a word8, to be seen
                            }

{-
REGARDING A SET OF VISITED STATES
A lot of memory.
Repeated states increase in higher depths.
Idea: save the 3 keys of (corners,edgesFst, edgesSnd). 3 word8 (3 bytes) per state.
-}                            

instance Show SearchingState where
    show (SearchingState f ini currD maxD _ sol layers _ lstFace _ _) = 
        "found: " ++ show f ++  "\n" ++
        "initial state: " ++ show ini ++  "\n" ++
        "current depth: " ++ show currD ++  "\n" ++
        "maximum depth: " ++ show maxD ++ "\n" ++
        "solution: " ++ show sol ++ "\n" ++
        "generation of moves: " ++ show layers ++
        "last face executed: " ++ show lstFace


-- | Recieves data, makes a generic bounded search and compose the solution
genericSearch :: BandagedCube               -- ^ Initial state
                -> (BandagedCube -> Bool)   -- ^ Condition to determine a Node is found
                -- -> [Face]                   -- ^ List of Turns to generate a new node
                -> [Turn]                   -- ^ List of Turns to generate a new node
                -> (BandagedCube -> Int)    -- ^ Heuristic (must be admissible)
                -> Maybe Algorithm          -- ^ The solution

genericSearch ini cond validMoves h
    | found search = Just (Algorithm (solution search))     --Solution found
    | otherwise = Nothing                                   --Solution not found
    where
        --nums = [1 .. 3]
        --gen1 = [ Turn(f, n) | f <- validLs, n <- nums]
        --To be improved: use only free faces (delete non movable)
        validLs = nub (map (\(Turn(f,_)) -> f ) validMoves)

        initialSS = SearchingState{found = False, initialState = ini,
                                    currentDepth = 0, maximumDepth = h ini, 
                                    condition = cond,
                                    solution = [], validLayers = validLs, 
                                    listMoves = validMoves, 
                                    lastFace = N, 
                                    heuristic = h,
                                    minimumExceding = 21}
                                    
                                    --Start initial max depth with heuristic of initial node
        search = idaStar initialSS

idaStar :: SearchingState -> SearchingState
idaStar initSS
    | found thisSearchSS = thisSearchSS
    | (nextDepth > treshold) = idaStar (initSS {maximumDepth = nextDepth})     --Update max depth with minimum node that exceeded the max.
    | otherwise = initSS
    where
        thisSearchSS = dfsSgle initSS
        treshold = maximumDepth initSS
        nextDepth = minimumExceding thisSearchSS

-- | Search with dfs from one node
dfsSgle :: SearchingState                           -- ^ Initial Searching State
            -> SearchingState                       -- ^ Final Searching State

dfsSgle initialSS
    | predicate ini = 
        initialSS {found = True}                                                --solution found       
    | currD > maxD || (currD + h ini > maxD) =                                --pruning, reached maximum depth
        prunedSS
    | otherwise =                                                               --intermediate, keep searching
        dfsMult initialSS movesToIterate
    where
        (SearchingState _ ini currD maxD predicate _ _ movesValid lstFace h exc) = initialSS
        estimLength = currD + h ini

        prunedSS = if ((estimLength > maxD) && (estimLength < exc))
            then
                (initialSS{minimumExceding = estimLength})
            else
                initialSS
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
    | isNothing nextState = dfsMult initialSS xs                    --Not valid turn, breaks a block
    | found thisBrach = thisBrach {solution = (x : solutionP)}      --Correct branch, recompose solution
    | currD > maxD = initialSS                                      --pruning (difficult with good heuristics)            
    | otherwise = 
        dfsMult (initialSS {minimumExceding = min exc0 maybeNewExc}) xs                              --Incorrect branch, keep searching
    where
        (SearchingState _ ini currD maxD _ _ _ _ _ _ exc0) = initialSS
        nextState = tryToTurn ini x
        (Turn(lastFaceExecuted, _)) = x
        thisBrach = dfsSgle (initialSS
            {initialState = fromJust nextState, 
            currentDepth = currD + 1,
            lastFace = lastFaceExecuted,
            minimumExceding = exc0
            } 
            )

        maybeNewExc = minimumExceding thisBrach
        solutionP = solution thisBrach
        