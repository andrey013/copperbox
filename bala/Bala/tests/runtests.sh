#!/bin/bash

from_dir=`pwd`
cd ../BalaExamples/


# test Midi, LilyPond and Abc generation are working
# - you need lilyond and abcm2ps intalled an in your path
echo "Running Riff.hs..."
runhaskell -i..:../HNotate:../ZMidi Riff.hs

# generate the midi file bulgarian6.midi
echo "Running Bulgarian6.hs..."
runhaskell -i..:../HNotate:../ZMidi Bulgarian6.hs

# read what you have written
echo "Running MidiPrint.hs..."
runhaskell -i..:../HNotate:../ZMidi MidiPrint.hs out/bulgarian6.midi > /dev/null

echo "Running Korda.hs..."
runhaskell -i..:../HNotate:../ZMidi Korda.hs

echo "Running DemoScore.hs..."
runhaskell -i..:../HNotate:../ZMidi DemoScore.hs


# annoying but the '--output=' directive for LilyPond isn't doing what I expect
# and we don't want the BalaExamples directory full of pdf and ps files 
mv *.pdf out/
mv *.ps out/ 

cd ../tests

echo "Running QCTests.hs..."
runhaskell -i..:../HNotate:../ZMidi QCTests.hs



echo "Done!"
cd ${from_dir}