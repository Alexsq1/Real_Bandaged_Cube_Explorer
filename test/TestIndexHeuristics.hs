module TestIndexHeuristics (testIndexHeuristics, genRealisticCube) where

import Test.QuickCheck

import Bandaged
import Cube
import TestCube         -- imports a generator of Bandaged
import MathematicalNotation
import Data.List
import Combinatorics
import IndexHeuristics
import CalculateKorfHeuristics


testIndexHeuristics :: IO()
testIndexHeuristics = do
    -- test of perfect hashes:
        quickCheck (perfectHashingPerms)
        quickCheck (perfectHashingNPR)
        quickCheck perfectHashingCP
        quickCheck perfectHashingEP
        quickCheck perfectHashingBC
    -- test of combinatorics:
        quickCheck factInverse1
        quickCheck factInverse2
        quickCheck nprInverse1
        quickCheck nprInverse2
        quickCheck coInverse1
        quickCheck coInverse2
        quickCheck eoInverse1
        quickCheck eoInverse2
        quickCheck mathNotationCorrect2

    -- test of index heuristics:
        quickCheck reorderEdgesAutoInverse         
        quickCheck calcAndRecompose1
        quickCheck calcAndRecompose2

    -- test of recompose (calculateKorfHeur)
        quickCheck recomposeCorrect

-- GENERATORS

-- | Generate n random CPs without repetitions
genCPs :: Int -> Gen [[Int]]
genCPs n = go n []
    where
        go 0 acc = return acc
        go k acc = do
            p <- gen1CP
            if p `elem` acc 
                then go k acc
                else go (k-1) (p:acc)

-- | Generate a random CPs (permutation of [0..7])
gen1CP :: Gen [Int]
gen1CP = shuffle [0 .. 7]

-- | Generate n random halves of EPs without repetitions
genEPHalves :: Int -> Gen [[Int]]
genEPHalves n = go n []
    where
        go 0 acc = return acc
        go k acc = do
            p <- gen1EP
            let p3 = take 6 p
            if p3 `elem` acc 
                then go k acc
                else go (k-1) (p3:acc)

-- | Generate a random EPs (permutation of [0..11])
gen1EP :: Gen [Int]
gen1EP = shuffle [0 .. 11]

-- | Generate a random CO that adds a multiple of 3
genCO :: Gen [Int]
genCO = do  
            xs <- vectorOf 7 (elements [0,1,2])
            let lastCorner = (3 - (sum xs) `mod` 3) `mod` 3
            return (xs ++ [lastCorner])
 
-- | Generate a random EO that adds a multiple of 2
genEO :: Gen [Int]
genEO = do
            xs <- vectorOf 11 (elements [0,1])
            let lastEdge = (2 - (sum xs) `mod` 2) `mod` 2
            return (xs ++ [lastEdge])

-- | Generate a realistic cube with proper orientation (might not have correct permutation)
-- To be deleted with another generator
genRealisticCube :: Gen Cube
genRealisticCube = do
                        cp <- gen1CP
                        ep <- gen1EP
                        co <- genCO
                        eo <- genEO
                        return (mathToCube (cp, co) (ep, eo))


-- TESTING COMBINATORICS

-- | The factorial encoding is a perfect hash function
perfectHashingPerms :: Property
perfectHashingPerms = property (sort numbering == [minimum numbering .. maximum numbering])
    where
        perms = permutations [0 .. 7]
        numbering = map (factorialEncode) perms

-- | The npr encoding is a perfect hash function
perfectHashingNPR :: Property
perfectHashingNPR = property (sort numbering == [minimum numbering .. maximum numbering])
    where
        vars = variations 6 [0..11]
        numbering = map (nprEncode) vars

        variations :: Int -> [Int] -> [[Int]]
        variations 0 _ = [[]]
        variations _ [] = [[]]
        variations k xs = [y:ys | (y,rest) <- select xs, ys <- variations (k-1) rest]
            where
                select [] = []
                select (x:xss) = (x,xss) : [(y,x:ys) | (y,ys) <- select xss]

-- | Two different CPs receive different hash values
perfectHashingCP :: Property
perfectHashingCP = forAll (genCPs 2000) $
                (\perms -> 
                    let indexs = map factorialEncode perms
                    in length (nub indexs) == length indexs)

-- | Two different EPs receive different hash values
perfectHashingEP :: Property
perfectHashingEP = forAll (genEPHalves 2000) $
                (\perms -> 
                    let indexs = map (nprEncode) perms
                    in length (nub indexs) == length indexs)

-- | Two different cubes receive different hash values
perfectHashingBC :: [BandagedCube] -> Property
perfectHashingBC bcList = let hashes = map (\(BandagedCube c _) -> (cornersKey c, edgesKeyFst c, edgesKeySnd c)) bcList
                        in property (length (nub bcList) == length (nub hashes))

-- | Inverses of factorial encode and decode (1)
factInverse1 :: Property
factInverse1 = forAll gen1CP $ \x ->
    ((factorialDecode) . (factorialEncode)) x == x

-- | Inverses of factorial encode and decode (2)
factInverse2 :: Int -> Property
factInverse2 n = n >= 0 && n < 40320 ==>
    (factorialEncode . factorialDecode) n == n

-- | Inverses of npr encode and decode (1, both halves)
nprInverse1 :: Property
nprInverse1 = forAll gen1EP $ \x ->
    (nprDecode . nprEncode) (take 6 x) == take 6 x &&
    (nprDecode . nprEncode) (drop 6 x) == drop 6 x

-- | Inverses of npr encode and decode (2)
nprInverse2 :: Int -> Property
nprInverse2 n = n >= 0 && n < 665280 ==>
    (nprEncode . nprDecode) n == n

-- | Inverses of CO encode and decode (1)
coInverse1 :: Property
coInverse1 = forAll genCO $ \x ->
    (decodeCO . encodeCO) x == x

-- | Inverses of CO encode and decode (2)
coInverse2 :: Int -> Property
coInverse2 n = n >= 0 && n < 2187 ==>
    (encodeCO . decodeCO) n == n

-- | Inverses of CP encode and decode (1)
eoInverse1 :: Property
eoInverse1 = forAll genEO $ \x ->
    (decodeEO . encodeEO) (take 6 x) == take 6 x &&
    (decodeEO . encodeEO) (drop 6 x) == drop 6 x

-- | Inverses of CP encode and decode (2)
eoInverse2 :: Int -> Property
eoInverse2 n = n >= 0 && n < 64 ==>
    (encodeEO . decodeEO) n == n

-- TESTING INDEXHEURISTICS

reorderEdgesAutoInverse :: Property
reorderEdgesAutoInverse = forAll gen1EP $ \ep ->
                            forAll genEO $ \eo ->
                            reorderEdges (reorderEdges (ep, eo)) == (ep, eo)


calcAndRecompose1 :: Cube -> Property
calcAndRecompose1 c = keysToCube (ck, e1, e2) === c
    where
        ck = cornersKey c
        e1 = edgesKeyFst c
        e2 = edgesKeySnd c

calcAndRecompose2 :: Cube -> Property
calcAndRecompose2 _ = forAll genRealisticIndices $ \(c, e1, e2) -> 
                        property (cornersKey (keysToCube (c, e1, e2)) == c && 
                                    edgesKeyFst (keysToCube (c, e1, e2)) == e1 && 
                                    edgesKeySnd (keysToCube (c, e1, e2)) == e2)
    where
        genRealisticIndices :: Gen (Int, Int, Int)
        genRealisticIndices = do
                                    --c <- arbitrary :: Cube
                                    c <- arbitrary
                                    let ck = cornersKey c
                                    let e1 = edgesKeyFst c
                                    let e2 = edgesKeySnd c
                                    return (ck, e1, e2)

mathNotationCorrect2 :: Property
mathNotationCorrect2 = forAll genValues $ \(cp, ep, co, eo, c) -> 
                            property (cornerState c == (cp, co) && edgesState c == (ep, eo))

    where
        genValues :: Gen ([Int], [Int], [Int], [Int], Cube) 
        genValues = do
                        cp <- gen1CP
                        ep <- gen1EP
                        co <- genCO
                        eo <- genEO
                        return (cp, ep, co, eo, mathToCube (cp,co) (ep,eo))

-- after recomposing, the numbers that match are the positions of the pieces of [0..5] or [6..11]
recomposeCorrect :: Cube -> Property
recomposeCorrect c = property ( 
                        -- in positions where ep1 is <= 5, ep1 == realEP , eo1 == realEO
                        map (\i -> ep1 !! i) interestingIndices1 == map (\i -> realEP !! i) interestingIndices1 &&
                        map (\i -> eo1 !! i) interestingIndices1 == map (\i -> realEO !! i) interestingIndices1 &&

                        -- in positions where ep2 is > 5, ep2 == realEP , eo2 == realEO
                        map (\i -> ep2 !! i) interestingIndices2 == map (\i -> realEP !! i) interestingIndices2 &&
                        map (\i -> eo2 !! i) interestingIndices2 == map (\i -> realEO !! i) interestingIndices2 && 
                        sort (interestingIndices1 ++ interestingIndices2) == [0..11])

    where
        (realEP, realEO) = edgesState c
        (_, k1, k2) = allKeys c
        c1 = recompose Edge1 k1
        c2 = recompose Edge2 k2
        (ep1, eo1) = edgesState c1
        (ep2, eo2) = edgesState c2
        interestingIndices1 = findIndices (<= 5) ep1
        interestingIndices2 = findIndices (> 5) ep2

