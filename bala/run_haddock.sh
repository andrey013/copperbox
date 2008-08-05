#!/bin/bash

bala_files=$(find Bala -name "*.hs" )

haddock -o doc/bala -h $bala_files

hnotate_files=$(find HNotate -name "*.hs" )

haddock -o doc/hnotate -h $hnotate_files

zmidi_files=$(find ZMidi -name "*.hs" )

haddock -o doc/zmidi -h $zmidi_files

