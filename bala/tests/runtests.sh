#!/bin/bash

from_dir=`pwd`
cd ../BalaExamples/


# generate the midi file bulgarian6.midi
runhaskell -i.. Bulgarian6.hs

# read what you have written
runhaskell -i.. MidiPrint.hs bulgarian6.midi > /dev/null

cd ../tests

runhaskell -i.. QCTests.hs



echo "Done!"
cd ${from_dir}