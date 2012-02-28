#!/bin/sh

ghc triplets.hs -o triplets 


cat triplets.hs | ./triplets -e vV | ./triplets -d -e vV | ./triplets -e dP | ./triplets -r dP -e rnm | ./triplets -d -e rnm
cat triplets.hs | ./triplets -e slash | ./triplets -r slash -e vV | ./triplets -d -r dP -e vV | ./triplets -d -e dP

./triplets foo
./triplets -e foo


cat triplets.hs | ./triplets > tmp
cat tmp | ./triplets -d > tmp.rc
rm tmp
rm tmp.rc

