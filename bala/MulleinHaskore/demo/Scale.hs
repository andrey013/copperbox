

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

-- start from g above middle c ( where middle c is c 5)

gMajNotes = line [g 5 qn [], a 5 qn [], b 5 qn [], c 6 qn [],
                  d 6 qn [], e 6 qn [], fs 6 qn [], g 6 qn [] ]

gMajor = Instr "piano" (Tempo 2 gMajNotes)


 
fourFourTime :: M.MetricalSpec
fourFourTime = M.metricalSpec 4 4

smap :: M.SpellingMap
smap = maybe (error "missing smap") id $ M.makeSpellingMap M.g_major []

main = simpleAbc "piano" M.g_major fourFourTime (M.buildSystem smap gMajor)

lyMain = writeFile "scale.ly" (show ly) where
    ly = simpleLilyPond "piano" M.g_major fourFourTime (M.buildSystem smap gMajor)


demoMidi = test gMajor


chord1 = Instr "piano" $ Tempo 2 $
           (g 5 qn []) :=: (b 5 qn []) :=: (d 5 qn [])

chordOut = simpleAbc "piano" M.g_major fourFourTime (M.buildSystem smap chord1)

chordOutLy = simpleLilyPond "piano" M.g_major fourFourTime (M.buildSystem smap chord1)


octD = M.octaveDist M.c5 M.g5