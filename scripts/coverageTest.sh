cabal clean && \
cabal build --enable-profiling && \
cabal test && \
hpc report Bandaged-Cube-TFG-test && \
hpc markup Bandaged-Cube-TFG-test && \

mv Bandaged-Cube-TFG-0.1.0.0-inplace-Bandaged-Cube-TFG-test coverage-Bandaged-Cube-TFG-test && \
mv hpc_*.html coverage-Bandaged-Cube-TFG-test && \
mv coverage-Bandaged-Cube-TFG-test profilings/coverage && \
rm Bandaged-Cube-TFG-test.tix
#(last 2, the name of the executable)