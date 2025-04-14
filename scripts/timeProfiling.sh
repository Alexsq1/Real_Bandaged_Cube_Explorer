cabal clean && \
cabal build --enable-profiling && \
cabal run exe:Bandaged-Cube-TFG -- +RTS -p && \
mv Bandaged-Cube-TFG.prof profilings/time
