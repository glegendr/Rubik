stack build && stack ghc src/Main.hs -- -O2 -threaded && rm src/*.o src/*.hi && mv src/Main Rubik