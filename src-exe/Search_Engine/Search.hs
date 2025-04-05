module Search(genericSearch) where

--import Cube
import Bandaged
import qualified Data.Set as S
import Moves
import Data.Maybe

data SearchingState = SearchingState {found :: Bool, 
                            initialState :: BandagedCube, 
                            currentDepth :: Int,
                            maximumDepth :: Int,
                            solution :: [Turn], 
                            searching :: [Turn], 
                            heuristic :: (BandagedCube -> Int),
                            visitedStates :: S.Set BandagedCube}
--                                deriving Show


genericSearch :: Int                        -- ^ Max depth
                -> BandagedCube             -- ^ Initial state
                -> (BandagedCube -> Bool)   -- ^ Condition to determine a Node is found
                -> [Turn]                   -- ^ List of Turns to generate a new node
                -> (BandagedCube -> Int)    -- ^ Heuristic (must be admissible)
                -> Maybe Algorithm          -- ^ The solution

genericSearch maxD ini condition genMoves h
    | found search = Just (Algorithm (solution search))
    | otherwise = Nothing
    where
        initialSS = SearchingState{found = False, initialState = ini,
                                    currentDepth = 0, maximumDepth = maxD, 
                                    solution = [], searching = genMoves, 
                                    heuristic = h, visitedStates = S.singleton ini}
        search = dfsSgle initialSS condition

dfsSgle :: SearchingState
            -> (BandagedCube -> Bool) 
            -> SearchingState

dfsSgle initialSS condition
    | ini `S.member` visited = initialSS                        --visited node                
    | currD > maxD || (currD + h ini > maxD) =                  --pruning, reached maximum depth
        initialSS {visitedStates = newSet}
    | condition ini = 
        initialSS {found = True, visitedStates = newSet}                       --found
    | otherwise =                                                   --intermediate, keep searching
        dfsMult nextSS condition genMoves

    where
        (SearchingState _ ini currD maxD _ genMoves h visited) = initialSS
        newSet = S.insert ini visited
        nextSS = initialSS {visitedStates = newSet, currentDepth = currentDepth initialSS + 1}

dfsMult :: SearchingState                   -- ^ Initial
            -> (BandagedCube -> Bool)       -- ^ Predicate
            -> [Turn]                           -- ^ List of moves used to generate branches
            -> SearchingState                  -- ^ Final

dfsMult initialSS _ [] = initialSS                   --ended iterating
dfsMult initialSS condition (x:xs)
    | isNothing nextState = dfsMult initialSS condition xs   --Not valid turn, breaks a block
    | found thisBrach = thisBrach {solution = (x:solutionP)}           --Correct branch
    | otherwise = dfsMult nextBranch condition xs                           --Incorrect branch, keep searching

    where
        (SearchingState _ ini _ _ _ _ _ visited) = initialSS
        nextState = tryToTurn ini x
        thisBrach = dfsSgle (initialSS{initialState = fromJust nextState}) condition

        (SearchingState _ _ _ _ solutionP _ _ visitedP) = thisBrach
        nextBranch = thisBrach {visitedStates = S.union visited visitedP}

--auxDFSMult :: BandagedCube -> BandagedCube -> [Move] -> Int -> Int -> S.Set BandagedCube -> Maybe [Move]
--auxDFSMult end start [] currentDepth maxBound visited = Nothing
--auxDFSMult end start (x:xs) currentDepth maxBound visited
--    | isNothing thisBrach = auxDFSMult end start xs currentDepth maxBound visited
--    | otherwise = Just (x : (fromJust thisBrach))
--    where
--        thisBrach = auxDFSSgle end (start <> (moveToPerm x)) (currentDepth + 1) maxBound (visited)
--
--

--boundedDFS :: BandagedCube -> BandagedCube -> Int -> Maybe [Move]
--boundedDFS end start bound = auxDFSSgle end start 0 bound S.empty
--    
--auxDFSSgle :: BandagedCube -> BandagedCube -> Int -> Int -> S.Set BandagedCube -> Maybe [Move]
--auxDFSSgle end start currentDepth maxBound visited
--    | currentDepth > maxBound || currentDepth > 20 = Nothing
--    | start `S.member` visited = Nothing
--    | end == start = Just []
--    | otherwise = auxDFSMult end start listPossibleMoves currentDepth maxBound (S.insert start visited)
--
----Cuidado: puede no acabar
--iddfs :: BandagedCube -> Maybe[Move]
--iddfs = iterDeep 0
--    where
--        iterDeep :: Int -> BandagedCube -> Maybe[Move]
--        iterDeep 20 _ = Nothing
--        iterDeep n c 
--            | isNothing currSearch = iterDeep (n+1) c
--            | otherwise = Just(fromJust (currSearch))
--            where
--                currSearch = boundedDFS mempty c n
--
--
--Ideas: compartir mejor el set de visitados
