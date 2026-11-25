# requires -prof -fprof-auto

cabal configure --enable-profiling --profiling-detail toplevel-functions && \
cabal clean && \
cabal build --enable-profiling && \
cabal run exe:Bandaged-Cube-TFG -- +RTS -p && \
cabal configure --disable-profiling && \
mv Bandaged-Cube-TFG.prof profilings/time
