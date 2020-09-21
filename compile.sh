stack build && stack ghc src/Main.hs -- -O2 -threaded && rm src/*.o src/*.hi
# if [ $0 -eq "run" || $0 -eq "execute" ]

if [ "$#" -ne 1 ]
then
    mv src/Main Rubik
elif [ $1 = "run" ] || [ $1 = "execute" ]
then
    mv src/Main Rubik && ./Rubik
fi