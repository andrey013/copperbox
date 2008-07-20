#!/bin/bash

from_dir=`pwd`
cd ../BalaExamples/


# test Midi, LilyPond and Abc generation are working
# - you need lilyond and abcm2ps intalled an in your path
runhaskell -i.. Riff.hs

# generate the midi file bulgarian6.midi
runhaskell -i.. Bulgarian6.hs

# read what you have written
runhaskell -i.. MidiPrint.hs bulgarian6.midi > /dev/null

runhaskell -i.. Korda.hs 

cd ../tests

runhaskell -i.. QCTests.hs



echo "Done!"
cd ${from_dir}