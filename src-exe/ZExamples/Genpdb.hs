module Genpdb where

import GenKorfHeuristics(genPDBs)
import Data.Word(Word8)


genpdb :: Word8 -> IO()
genpdb n = genPDBs n
