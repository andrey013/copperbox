#!/bin/bash

from_dir=`pwd`
cd ../BalaExamples/


# test Midi, LilyPond and Abc generation are working
# - you need lilyond and abcm2ps intalled an in your path
runhaskell -i.. Riff.hs

# generate the midi file bulgarian6.midi
runhaskell -i.. Bulgarian6.hs

# read what you have written
runhaskell -i.. MidiPrint.hs out/bulgarian6.midi > /dev/null

runhaskell -i.. Korda.hs

# annoying but the '--output=' directive for LilyPond isn't doing what I expect
# and we don't want the BalaExamples directory full of pdf and ps files 
mv *.pdf out/
mv *.ps out/ 

cd ../tests

runhaskell -i.. QCTests.hs



echo "Done!"
cd ${from_dir}