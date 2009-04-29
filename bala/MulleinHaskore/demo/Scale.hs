

-- :set -i../src:../../Mullein/src


module Scale where

import Haskore
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

demo0 = test gMajor

demo1' = M.untree 0 "no-inst" gMajor

ovs1 :: M.OverlayList M.ScNote
ovs1 = maybe (error "missing") M.mergeParallels 
          $ Map.lookup "piano" $ M.parSystem "no-inst" gMajor

score1 = M.part [M.phrase $ M.motif M.g_major fourFourTime ovs1] 
 
fourFourTime :: M.MetricalSpec
fourFourTime = M.metricalSpec 4 4


gMajor_abc :: M.Part
gMajor_abc = M.convertToAbc smap M.du8 score1 where
  smap = maybe (error "spelling map missing") id $ M.makeSpellingMap M.g_major []

main = putDoc $ M.getAbcOutput $ 
         M.generateAbc M.g_major (fst fourFourTime) (repeat 4) gMajor_abc


chord1 = (g 4 qn []) :=: (b 4 qn []) :=: (d 4 qn [])

{-
 
fourFourTime :: M.MetricalSpec
fourFourTime = M.metricalSpec 4 4

gMajor_melody = f $ snd $ head sys1 where
    f    = M.melody M.g_major fourFourTime 
    sys1 = M.system' gMajor

gMajor_abc :: M.Part
gMajor_abc = M.convertToAbc lset M.eighth gMajor_melody where
  lset = maybe (error "lset missing") id $ M.makeLabelSet M.g_major

gMajor_ly :: M.Part
gMajor_ly = M.convertToLy M.c4 gMajor_melody where
  lset = maybe (error "lset missing") id $ M.makeLabelSet M.g_major



abcPrint = putDoc $ M.outputAbc M.g_major (fst fourFourTime) (repeat 4) gMajor_abc

lyPrint  = putDoc $ M.outputLy M.g_major (fst fourFourTime) gMajor_ly

-}