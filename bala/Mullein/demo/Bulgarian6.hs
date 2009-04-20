
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- ghci ...
-- :set -i../src

module Bulgarian6 where

import Mullein.AbcConvert
import qualified Mullein.AbcOutput as Abc
import Mullein.Bracket
import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.LabelSet
import Mullein.NamedElements ( sixteenth, a_major, c4' )
import Mullein.Pitch
import Mullein.RS
import Mullein.Score
import Mullein.Utils

import qualified Mullein.LilyPondConvert as Ly
import qualified Mullein.LilyPondOutput as Ly

import Data.Ratio
import Text.PrettyPrint.Leijen ( putDoc )


main = putDoc $ Abc.output a_major (TimeSig 2 4) (repeat 4) bulgarian6

ly = putDoc $ Ly.output a_major bulg6 where
  bulg6 = Ly.convertToLy Ly.relPitch c4' tune1


bulgarian6 :: Part
bulgarian6 = convertToAbc lset sixteenth tune1 where
  lset = maybe (error "lset missing") id $ makeLabelSet a_major

tune1 = evaluatePart a_major twoFourTime part1_8


part1_8 :: NoteCtx Part
part1_8 = part [repeated m1_4, repeated m5_8]



m1_4 :: NoteCtx Motif
m1_4 = motif $ primary bars1_4

m5_8 :: NoteCtx Motif
m5_8 = motif $ primary bars5_8



twoFourTime :: MetricalSpec
twoFourTime = metricalSpec 2 4


bars1_4 :: NoteCtx [Element]
bars1_4 = notelist $ 
           [ a4 & du16, b4, cis5, cis5, cis5, a4, cis5, cis5
           , cis5, a4, b4, cis5, b4, a4, a4, rest        
           , e5, d5, cis5, b4, cis5, a4, b4, cis5
           , a4, b4, b4, a4 , a4 & du8, rest    
           ]

bars5_8 :: NoteCtx [Element]
bars5_8 = notelist $ 
           [ c5 & du8, b4 & du16, a4, b4 & du8, a4 & du16, gis4
           , fis4, e4, fis4, gis4, a4 & du8, b4
           , cis5, b4 & du16, a4, b4 & du8, a4 & du16, gis4
           , fis4, e4, fis4 & du8, rest
           ]




    