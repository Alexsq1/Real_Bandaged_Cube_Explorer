module Search(genericSearch, SearchingState(..), extractAlg, digestSearch, SolutionInfo) where

import Bandaged
import Moves
import Data.Maybe(fromJust)
import CubeCreator(newSolvedCube)

-- | SearchingState storages all the information needed to to a Search.
data SearchingState = SearchingState {
                    --Arguments
                            initialState :: BandagedCube, 
                            condition :: (BandagedCube -> Bool),
                            listMoves :: [Turn], 
                            heuristic :: (BandagedCube -> Int),
                    --Internal data
                            found :: Bool, 
                            currentDepth :: Int,
                            maximumDepth :: Int,
                            minimumExceding :: Int,      --Maybe a word8, to be seen
                            solution :: [Turn], 
                            lastFace :: Face,
                    --Statistical
                            numVisited :: Integer,
                            leafs :: Integer,
                            numPruned :: Integer,
                            exploredDepths :: [Int]
                            }

instance Show SearchingState where
    show (SearchingState _ _ moves _ f currD maxD minExc sol _ visit lf pr depths) =
        --"initial state: " ++ show ini ++  "\n" ++
        "generation of moves: " ++ show moves ++ "\n" ++ 
        "found: " ++ show f ++  "\n" ++
        "current depth: " ++ show currD ++  "\n" ++
        "maximum depth: " ++ show maxD ++ "\n" ++
        "minimum exceeding: " ++ show minExc ++ "\n" ++
        "solution: " ++ show sol ++ "\n" ++
        --"last face executed: " ++ show lstFace ++ "\n" ++ 
        "visited states: " ++ show visit ++ "\n" ++ 
        "leaf states: " ++ show lf ++ "\n" ++ 
        "pruned states: " ++ show pr ++ "\n" ++ 
        "explored dephts: " ++ show depths
        --validLs = nub (map (\(Turn(f,_)) -> f ) moves)

data SolutionInfo = SolutionInfo{
                    solutionSI :: Algorithm, 
                    lAlg :: Int,
                    sizeTree :: Integer,
                    visited :: Integer,
                    branchingFactor :: Float,
                    exploredRatio :: Float,
                    prunedRatio :: Float,
                    exploredDepthsSI :: [Int]
}

instance Show SolutionInfo where
    show (SolutionInfo sol la sizeT v bf expl pr depths) = 
        "algorithm: " ++ show sol ++ "\n" ++
        "of length: " ++ show la ++ "\n" ++
        "size of the searching tree: " ++ show sizeT ++ "\n" ++
        "visited: " ++ show v ++ "\n" ++
        "estimated branching factor: " ++ show bf ++ "\n" ++
        "ratio of explored states: " ++ show expl ++ "\n" ++
        "ratio of pruned states: " ++ show pr ++ "\n" ++
        "explored depths: " ++ show depths ++ "\n"

digestSearch :: SearchingState -> SolutionInfo
digestSearch (SearchingState (BandagedCube _ blocks) _ listM _ _ _ _ _ sol _ visit _ pr depths) =
    SolutionInfo {
                    solutionSI = Algorithm sol,
                    lAlg = length sol,
                    sizeTree = sizeT,
                    visited = visit,
                    branchingFactor = estimBF,
                    exploredRatio = expR,
                    prunedRatio = (fromIntegral pr) / (fromIntegral visit),
                    exploredDepthsSI = depths
    }
    where
        n = 4
        xs = [n, n + 1]
        ss = map (\x -> dfsSgle
            SearchingState {
                initialState = BandagedCube {stdCube = newSolvedCube, restrictions = blocks},
                condition = const False,
                listMoves = listM,
                heuristic = const 0,
                found = False,
                currentDepth = 0,
                maximumDepth = x,
                minimumExceding = 0,
                solution = [],
                lastFace = N,
                numVisited = 0,
                leafs = 0,
                numPruned = 0,
                exploredDepths = []
            }
            ) xs
        sizesDepths = map (fromIntegral . leafs) ss
        estimBF = (sizesDepths !! 1) / (sizesDepths !! 0)
        sizeT = floor(estimBF ^ (length sol))
        expR = (fromIntegral visit) / (fromIntegral sizeT)

-- | Extracts the alg from a solution info
extractAlg :: SolutionInfo -> Algorithm
extractAlg = solutionSI


-- | Recieves data, makes a generic bounded search and compose the solution
genericSearch :: BandagedCube               -- ^ Initial state
                -> (BandagedCube -> Bool)   -- ^ Condition to determine a Node is found
                -> [Turn]                   -- ^ List of Turns to generate a new node
                -> (BandagedCube -> Int)    -- ^ Heuristic (must be admissible)
                -> Maybe SolutionInfo        -- ^ The solution

genericSearch ini cond validMoves h
    | found search = Just (digestSearch search)                --Solution found
    | otherwise = Nothing                                      --Solution not found
    where
        initialSS = SearchingState{
                                    initialState = ini,
                                    condition = cond,
                                    listMoves = validMoves, 
                                    heuristic = h,
                                    found = False,
                                    currentDepth = 0,
                                    maximumDepth = h ini, 
                                    minimumExceding = maxBound :: Int,
                                    solution = [], 
                                    lastFace = N,
                                    numVisited = 0,
                                    leafs = 0,
                                    numPruned = 0,
                                    exploredDepths = [h ini]
                                    }                          
        search = idaStar initialSS

idaStar :: SearchingState -> SearchingState
idaStar initSS
    | found thisSearchSS = thisSearchSS
    | (nextDepth > currMaxDepth) = idaStar (initSS                                      --Update max depth with minimum node that exceeded the max.
                                        {maximumDepth = nextDepth,
                                        exploredDepths = listDepths ++ [nextDepth]
                                        })
    | otherwise = initSS
    where
        (SearchingState _ _ _ _ _ _ currMaxDepth _ _ _ _ _ _ listDepths) = initSS
        thisSearchSS = dfsSgle initSS
        nextDepth = minimumExceding thisSearchSS

-- | Search with dfs from one node
dfsSgle :: SearchingState                           -- ^ Initial Searching State
            -> SearchingState                       -- ^ Final Searching State

dfsSgle initialSS
    | predicate ini = 
        initialSS {found = True, numVisited = 1 + v}                      --solution found       
    | currD > maxD =                                                            --reached maximum depth (impossible?)
        prunedSS
    | (estimLength > maxD) =                                                    --pruning, reached maximum depth
        prunedSS
    | currD == maxD =                                                           --maxD and condition not satisfied, stop searching
        initialSS {numVisited = 1 + v, leafs = 1 + lf}
    | otherwise =                                                               --intermediate, keep searching
        dfsMult initialSS{numVisited = 1 + v} movesToIterate
    where
        (SearchingState ini predicate movesValid h _ currD maxD exc _ lstFace v lf pr _) = initialSS
        estimLength = currD + h ini

        prunedSS = if ((estimLength > maxD) && (estimLength < exc))
            then
                (initialSS{minimumExceding = estimLength, numVisited = 1 + v, numPruned = 1 + pr})
            else
                initialSS
        movesToIterate = filter (predValidCanonicSequence ini lstFace) movesValid

        --This makes canonical sequences
        
        predValidCanonicSequence :: BandagedCube -> Face -> Turn -> Bool
        predValidCanonicSequence bc lsface (Turn(f,_)) = 
            ((axisOfFace f /= axisOfFace lsface) || (f > lsface)) &&
            (validTurn bc f)

-- | Search with dfs algorithm. Iterate over several move generation
dfsMult :: SearchingState                       -- ^ Initial
            -> [Turn]                           -- ^ List of turns used to generate branches
            -> SearchingState                   -- ^ Final

dfsMult initialSS [] = initialSS                                    --ended iterating
dfsMult initialSS (x:xs)                                            --keep iterations
    | found thisBrach = thisBrach {solution = (x : solutionP)}      --Correct branch, recompose solution
    | otherwise =                                                   --Incorrect branch, keep searching
        dfsMult (initialSS {minimumExceding = min exc0 maybeNewExc,
                            numVisited = numVisited thisBrach,
                            numPruned = numPruned thisBrach,
                            leafs = leafs thisBrach
        }) xs
    where
        (SearchingState ini _ _ _ _ currD _ exc0 _ _ _ _ _ _) = initialSS
        nextState = tryToTurn ini x
        (Turn(lastFaceExecuted, _)) = x
        thisBrach = dfsSgle (initialSS
            {initialState = fromJust nextState, 
            currentDepth = currD + 1,
            lastFace = lastFaceExecuted,
            minimumExceding = exc0
            })

        maybeNewExc = minimumExceding thisBrach
        solutionP = solution thisBrach
        