stack build && stack ghc src/Main.hs -- -O2 -threaded && rm src/*.o src/*.hi && mv src/Main Rubik

# if [ $0 -eq "run" || $0 -eq "execute" ]
if [ $1 = "run" ] || [ $1 = "execute" ]
then
    ./Rubik
fi