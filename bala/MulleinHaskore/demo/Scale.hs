

-- :set -i../src:../../Mullein/src


module Scale where

import Haskore

import MulleinHaskore.Abc
import MulleinHaskore.LilyPond
import qualified MulleinHaskore.System as M

import qualified Mullein.AbcConvert       as M
import qualified Mullein.AbcOutput        as M
import qualified Mullein.Core             as M
import qualified Mullein.CoreTypes        as M
import qualified Mullein.LilyPondConvert  as M
import qualified Mullein.LilyPondOutput   as M
import qualified Mullein.NamedElements    as M
import qualified Mullein.Pitch            as M
import qualified Mullein.Score            as M
import qualified Mullein.SpellingMap      as M 

import qualified Data.Map as Map
import Text.PrettyPrint.Leijen ( putDoc )


gMajNotes = line [g 4 qn [], a 4 qn [], b 4 qn [], c 5 qn [],
                  d 5 qn [], e 5 qn [], fs 5 qn [], g 5 qn [] ]

gMajor = Instr "piano" (Tempo 2 gMajNotes)


 
fourFourTime :: M.MetricalSpec
fourFourTime = M.metricalSpec 4 4


main = simpleAbc "piano" M.g_major fourFourTime (M.buildSystem gMajor)

lyMain = simpleLilyPond "piano" M.g_major fourFourTime (M.buildSystem gMajor)


demoMidi = test gMajor


chord1 = Instr "piano" $ Tempo 2 $
           (g 4 qn []) :=: (b 4 qn []) :=: (d 4 qn [])

chordOut = simpleAbc "piano" M.g_major fourFourTime (M.buildSystem chord1)

chordOutLy = simpleLilyPond "piano" M.g_major fourFourTime (M.buildSystem chord1)

