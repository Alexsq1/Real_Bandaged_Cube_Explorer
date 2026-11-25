# Requires -prof -fprof-auto

cabal clean && \
cabal configure --enable-profiling --profiling-detail=toplevel-functions && \
cabal build --enable-profiling && \
#cabal run exe:Bandaged-Cube-TFG -- +RTS -hc && \   #not bad. Bastante genérico
#cabal run exe:Bandaged-Cube-TFG -- +RTS -hy  && \  #Interesante. Divide más en algunas funciones propias
cabal run exe:Bandaged-Cube-TFG -- +RTS -hm  && \  #Muy interesante, por módulos (el bueno) (siempre funcionó)
#cabal run exe:Bandaged-Cube-TFG -- +RTS -hb  && \ #Parecido, nombres no tan significativos
cabal configure --disable-profiling && \

hp2ps -c Bandaged-Cube-TFG.hp && \
ps2pdf Bandaged-Cube-TFG.ps && \
#mv Bandaged-Cube-TFG.ps profilings/memory && \
#xdg-open profilings/memory/Bandaged-Cube-TFG.pdf && \

rm Bandaged-Cube-TFG.ps && \
rm Bandaged-Cube-TFG.hp && \
rm Bandaged-Cube-TFG.aux && \

mv Bandaged-Cube-TFG.pdf profilings/memory

