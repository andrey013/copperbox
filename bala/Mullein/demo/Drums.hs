
-- Write LilyPond drum pitches

-- ghci ...
-- :set -i../src

module Drums where

import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.LilyPondConvert
import Mullein.LilyPondOutput
import Mullein.LilyPondPercussion
import Mullein.NamedElements
import Mullein.Score

import Text.PrettyPrint.Leijen ( putDoc )


main = putDoc $ getLilyPondOutput $ 
         generateLilyPond c_major (fst fourFourTime) drums where
  drums = convertToLyAbsolute drum_part

dMotif = motif c_major fourFourTime

fourFourTime :: MetricalSpec
fourFourTime = metricalSpec 4 4

drum_part :: PartP DrumPitch
drum_part = part [repeated drum_motif]


drum_motif :: MotifP DrumPitch
drum_motif = dMotif $ primary drum_bars


drum_bars :: [ElementP DrumPitch]
drum_bars = [ snare qn, qnr, snare qn, qnr ]
