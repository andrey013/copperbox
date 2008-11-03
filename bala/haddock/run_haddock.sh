#!/bin/bash


haddock --odir=doc/bala   --html --optghc=-i../HNotate:../Bala:../ZMidi BalaHaddock.hs
haddock --odir=doc/zmidi  --html --optghc=-i../ZMidi ZMidiHaddock.hs
haddock --odir=doc/hnotate  --html --optghc=-i../HNotate HNotateHaddock.hs

# move up one to find the hi and o files
cd ..

HI_FILES=$(find -name "*.hi" )
O_FILES=$(find -name "*.o" )


for file in $HI_FILES
do
  mv $file ./haddock/_trash/
done


for file in $O_FILES
do
  mv $file ./haddock/_trash/
done

# move back 
cd ./haddock



