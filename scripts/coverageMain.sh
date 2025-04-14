cabal clean && \
cabal build --enable-profiling && \
cabal run && \
hpc report Bandaged-Cube-TFG && \
hpc markup Bandaged-Cube-TFG && \

mv Bandaged-Cube-TFG-0.1.0.0-inplace-Bandaged-Cube-TFG coverage-Bandaged-Cube-TFG && \
mv hpc_*.html coverage-Bandaged-Cube-TFG && \
mv coverage-Bandaged-Cube-TFG profilings/coverage && \
rm Bandaged-Cube-TFG.tix