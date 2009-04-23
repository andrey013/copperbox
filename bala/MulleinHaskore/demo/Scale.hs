

-- :set -i../src:../../Mullein/src


module Scale where

import Haskore
import qualified MulleinHaskore.System as M

import qualified Mullein.AbcConvert    as M
import qualified Mullein.AbcOutput     as M
import qualified Mullein.Core          as M
import qualified Mullein.CoreTypes     as M
import qualified Mullein.LabelSet      as M
import qualified Mullein.NamedElements as M

import Text.PrettyPrint.Leijen ( putDoc )


gMajNotes = line [g 4 qn [], a 4 qn [], b 4 qn [], c 5 qn [],
                  d 5 qn [], e 5 qn [], fs 5 qn [], g 5 qn [] ]

gMajor = Instr "piano" (Tempo 2 gMajNotes)

main = test gMajor

fourFourTime :: M.MetricalSpec
fourFourTime = M.metricalSpec 4 4

gMajor_melody = f $ snd $ head sys1 where
    f    = M.melody M.g_major fourFourTime 
    sys1 = M.system' gMajor

gMajor_abc :: M.Part
gMajor_abc = M.convertToAbc lset M.eighth gMajor_melody where
  lset = maybe (error "lset missing") id $ M.makeLabelSet M.g_major

abcPrint = putDoc $ M.output M.g_major (fst fourFourTime) (repeat 4) gMajor_abc

