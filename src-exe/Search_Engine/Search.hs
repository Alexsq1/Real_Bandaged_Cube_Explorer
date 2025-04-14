module Search(genericSearch, idaStar, SearchingState(..), dfsSgle, dfsMult) where

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
                            listSubalgs :: [[Turn]], 
                            heuristic :: (BandagedCube -> Int)}
                            
                            --to be added: min node that surpassed the bound

instance Show SearchingState where
    show (SearchingState f ini currD maxD _ sol layers _ _) = 
        "found: " ++ show f ++  "\n" ++
        "initial state: " ++ show ini ++  "\n" ++
        "current depth: " ++ show currD ++  "\n" ++
        "maximum depth: " ++ show maxD ++ "\n" ++
        "solution: " ++ show sol ++ "\n" ++
        "generation of moves: " ++ show layers
        
        
-- | A list with all the possible 1-face moves
--allPossibleMoves :: [Turn]
--allPossibleMoves = [Turn(f, m) | f <- [R .. ] , m <- [1..3]]

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
        gen1 = [ [Turn(f, n)] | f <- validLs, n <- nums]
--        gen2 = [ [Turn (f1,n1), Turn (f2,n2)] | f1 <- validLs, f2 <- validLs , n1 <- nums, n2 <- nums, (axisOfFace f1) == (axisOfFace f2), f1 < f2]

        initialSS = SearchingState{found = False, initialState = ini,
                                    currentDepth = 0, maximumDepth = 0, 
                                    condition = cond,
                                    solution = [], validLayers = validLs, 
                                    listSubalgs = gen1, heuristic = h}
                                    --listSubalgs = gen1 ++ gen2
        search = idaStar initialSS


idaStar :: SearchingState -> SearchingState
idaStar initSS
    | found thisSearchSS = thisSearchSS
    | (treshold <= 20) = idaStar (initSS {maximumDepth = treshold + 1})
    | otherwise = initSS
    where
        thisSearchSS = dfsSgle initSS
        treshold = maximumDepth initSS

-- | Search with dfs from one node
dfsSgle :: SearchingState                           -- ^ Initial Searching State
            -> SearchingState                       -- ^ Final Searching State

dfsSgle initialSS
    | (condition initialSS) ini = 
        initialSS {found = True}                                                --solution found       
    | currD >= maxD || (currD + h ini >= maxD) =                                --pruning, reached maximum depth
        initialSS
    | otherwise =                                                               --intermediate, keep searching
        dfsMult initialSS (subAlgs)

    where
        (SearchingState _ ini currD maxD _ _ layers subAlgs h) = initialSS
        
-- | Search with dfs algorithm. Iterate over several move generation
dfsMult :: SearchingState                       -- ^ Initial
            -> [[Turn]]                         -- ^ List of (sub)algs used to generate branches
            -> SearchingState                   -- ^ Final

dfsMult initialSS [] = initialSS                                    --ended iterating
dfsMult initialSS (x:xs)                                            --keep iterations
    | currD > maxD = initialSS                                      --pruning (difficult with good heuristics)            
    | isNothing nextState = dfsMult initialSS xs                    --Not valid turn, breaks a block
    | found thisBrach = thisBrach {solution = (x ++ solutionP)}     --Correct branch, recompose solution
    | otherwise = dfsMult initialSS xs                              --Incorrect branch, keep searching

    where
        (SearchingState _ ini currD maxD _ _ layers _ _) = initialSS
        nextState = tryToExecuteAlg ini (Algorithm x)

--        nextState = tryToTurn ini x
--        thisBrach = dfsSgle (initialSS{initialState = fromJust nextState, currentDepth = currentDepth initialSS + 1})

        (Turn(lastFace, _)) = last x
        --adjFaces = filter (\l -> axisOfFace l /= axisOfFace lastFace) (layers)
        --adjFaces
        nums = [1 .. 3]

        next = [ [Turn (f,n)] | f <- layers, n <- nums, f /= lastFace]
        --follow1 = [ [Turn (f,n)] | f <- adjFaces , n <- nums]
        --follow2 = [ [Turn (f1,n1), Turn (f2,n2)] | f1 <- adjFaces, f2 <- adjFaces , n1 <- nums, n2 <- nums, 
        --    (axisOfFace f1) == (axisOfFace f2), f1 < f2]

        thisBrach = dfsSgle (initialSS{initialState = fromJust nextState, 
            currentDepth = currD + (length x), listSubalgs = (next)})

        --thisBrach = dfsSgle (initialSS{initialState = fromJust nextState, 
        --    currentDepth = currD + (length x), listSubalgs = (follow1 ++ follow2)})

        (SearchingState _ _ _ _ _ solutionP _ _ _) = thisBrach
        
