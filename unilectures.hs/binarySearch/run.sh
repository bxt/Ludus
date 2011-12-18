#!/bin/sh

echo "HLint output:"
hlint .

echo "QuickTest"
runhaskell Test.hs

echo "Configuring for local"
cabal configure --user
echo "Compiling"
cabal build
echo "Generating docs"
cabal haddock --executables --internal --hyperlink-source
echo "Running Main.hs"
./dist/build/ads-binarySearch/ads-binarySearch

# cabal clean
