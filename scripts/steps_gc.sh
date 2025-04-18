cabal clean && \
cabal build --enable-profiling && \
cabal run exe:Bandaged-Cube-TFG -- +RTS -s 2> profilings/garbage_collector_steps/gc_steps.txt
#mv Bandaged-Cube-TFG.prof profilings/time
