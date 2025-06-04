module TestMoves (testsMoves) where

import Test.QuickCheck
import Moves
--import Test.QuickCheck.Gen
import Data.Group

testsMoves :: IO()
testsMoves = do
    quickCheck (associativityMoves)
    quickCheck (neutral1)
    quickCheck (neutral2)
    quickCheck (inverse1)
    quickCheck (inverse2)
    quickCheck (inversedoble)

associativityMoves :: Algorithm -> Algorithm -> Algorithm -> Property
associativityMoves a1 a2 a3 = (a1 <> (a2 <> a3)) === ((a1 <> a2) <> a3)

neutral1 :: Algorithm -> Property
neutral1 a = (a <> mempty) === a

neutral2 :: Algorithm -> Property
neutral2 a = (mempty <> a) === a

inverse1 :: Algorithm -> Property
inverse1 a = (a <> (invert a)) === mempty

inverse2 :: Algorithm -> Property
inverse2 a = ((invert a) <> a) === mempty

inversedoble :: Algorithm -> Property
inversedoble a = ( (invert . invert) a) === a