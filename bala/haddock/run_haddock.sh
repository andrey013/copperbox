#!/bin/bash


haddock --odir=doc/bala   --html --optghc=-i../HNotate:../Bala:../ZMidi BalaHaddock.hs
haddock --odir=doc/zmidi  --html --optghc=-i../ZMidi ZMidiHaddock.hs
haddock --odir=doc/hnotate  --html --optghc=-i../HNotate HNotateHaddock.hs

HI_FILES=$(find -name "*.hi" )
O_FILES=$(find -name "*.o" )


for file in $HI_FILES
do
  mv $file _trash/
done


for file in $O_FILES
do
  mv $file _trash/
done



