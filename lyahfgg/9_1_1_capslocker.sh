#!/bin/zsh

ghc --make 9_0_1_continuousReverse.hs
ghc --make 9_1_1_capslocker.hs

cat data.txt | ./9_0_1_continuousReverse
cat data.txt | ./9_1_1_capslocker

rm 9_0_1_continuousReverse
rm 9_1_1_capslocker
