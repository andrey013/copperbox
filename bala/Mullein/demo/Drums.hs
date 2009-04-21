
-- Write LilyPond drum pitches

-- ghci ...
-- :set -i../src

module Drums where

import Mullein.Core
import Mullein.CoreTypes
import Mullein.LilyPondConvert
import Mullein.LilyPondOutput hiding ( repeated )
import Mullein.LilyPondPercussion
import Mullein.NamedElements ( a_major, c4' )
import Mullein.Score

import Text.PrettyPrint.Leijen ( putDoc )


main = putDoc $ output a_major drums where
  drums = convertToLy c4' drums1


drums1 = evaluatePart a_major fourFourTime drum_part


fourFourTime :: MetricalSpec
fourFourTime = metricalSpec 4 4

drum_part :: NoteCtx (PartP DrumPitch)
drum_part = part [repeated drum_motif]


drum_motif :: NoteCtx (MotifP DrumPitch)
drum_motif = motif $ primary drum_bars


drum_bars :: NoteCtx [ElementP DrumPitch]
drum_bars = notelist $ 
           [ drum snare, rest, drum snare,rest
           ]
