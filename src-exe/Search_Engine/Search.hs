module Search(genericSearch, idaStar, SearchingState(..), dfsSgle, dfsMult, allPossibleMoves) where

--import Cube
import Bandaged
import qualified Data.Set as S
import Moves
import Data.Maybe

-- | SearchingState storages all the information needed to to a Search.
data SearchingState = SearchingState {found :: Bool, 
                            initialState :: BandagedCube, 
                            currentDepth :: Int,
                            maximumDepth :: Int,
                            condition :: (BandagedCube -> Bool),
                            solution :: [Turn], 
                            searching :: [Turn], 
                            heuristic :: (BandagedCube -> Int),
                            visitedStates :: S.Set BandagedCube}
--                                deriving Show
                            --to be added: min node that surpassed the bound

instance Show SearchingState where
    show (SearchingState f ini currD maxD _ sol genMoves _ visited) = 
        "found: " ++ show f ++  "\n" ++
        "initial state: " ++ show ini ++  "\n" ++
        "current depth: " ++ show currD ++  "\n" ++
        "maximum depth: " ++ show maxD ++ "\n" ++
        "solution: " ++ show sol ++ "\n" ++
        "generation of moves: " ++ show genMoves ++  "\n" ++
        "number of visited states: " ++ show (S.size visited)

-- | A list with all the possible 1-face moves
allPossibleMoves :: [Turn]
allPossibleMoves = [Turn(f, m) | f <- [R .. ] , m <- [1..3]]

-- | Recieves data, makes a generic bounded search and compose the solution
genericSearch :: BandagedCube               -- ^ Initial state
                -> (BandagedCube -> Bool)   -- ^ Condition to determine a Node is found
                -> [Turn]                   -- ^ List of Turns to generate a new node
                -> (BandagedCube -> Int)    -- ^ Heuristic (must be admissible)
                -> Maybe Algorithm          -- ^ The solution



genericSearch ini cond genMoves h
    | found search = Just (Algorithm (solution search))     --Solution found
    | otherwise = Nothing                                   --Solution not found
    where
        initialSS = SearchingState{found = False, initialState = ini,
                                    currentDepth = 0, maximumDepth = 0, 
                                    condition = cond,
                                    solution = [], searching = genMoves, 
                                    heuristic = h, visitedStates = S.empty}
        search = idaStar initialSS

--The visited states set is not useful.
--More useful: check that the current alg has no cycles (sub-algs do a non-trivial permutation).
--Other optimiziation option: when exploring 1 face, delete it from the next turn. Do something similar with paralel layers. (branch factor 18 -> 13.5)


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
        initialSS {found = True}                        --solution found
--    | ini `S.member` visited = initialSS                                        --visited node.                
    | currD >= maxD || (currD + h ini >= maxD) =                                --pruning, reached maximum depth
        initialSS
    | otherwise =                                                               --intermediate, keep searching
        dfsMult nextSS genMoves

    where
        (SearchingState _ ini currD maxD _ _ genMoves h visited) = initialSS
        newSet = S.insert ini visited
        nextSS = initialSS

-- | Search with dfs algorithm. Iterate over several move generation
dfsMult :: SearchingState                       -- ^ Initial
            -> [Turn]                           -- ^ List of moves used to generate branches
            -> SearchingState                   -- ^ Final

dfsMult initialSS [] = initialSS                                    --ended iterating
dfsMult initialSS (x:xs)                                            --keep iterations
    | currD > maxD = initialSS                                      --pruning (difficult with good heuristics)            
    | isNothing nextState = dfsMult initialSS xs                    --Not valid turn, breaks a block
    | found thisBrach = thisBrach {solution = (x:solutionP)}        --Correct branch, recompose solution
    | otherwise = dfsMult nextBranch xs                             --Incorrect branch, keep searching

    where
        cond = condition initialSS
        (SearchingState _ ini currD maxD _ _ _ _ visited) = initialSS
        nextState = tryToTurn ini x
        thisBrach = dfsSgle (initialSS{initialState = fromJust nextState, currentDepth = currentDepth initialSS + 1})

        (SearchingState _ _ _ _ _ solutionP _ _ visitedP) = thisBrach
        nextBranch = initialSS
