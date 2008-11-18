
-- Note to me...
-- What about visual intrepretations, e.g. so we can see how well
-- a sequence fits is metrical shape?


-- ghci ...
-- :set -i../../HNotate:../../Bala:../../ZMidi

module MeterDemo where

import Bala.Base.OutputMidi


import Bala.Base hiding (a4)


import ZMidi (writeMidi)



import qualified Data.Foldable as F
import Data.List hiding (transpose, null)
import Data.Ratio
import Data.Sequence
import Prelude hiding (null)


attack d = note c4 d

a4    = attack du4
a4'   = attack (dotn 1 du4)
r4    = rest du4
a8    = attack du8
r8    = rest du8
a8'   = attack (dotn 1 du8)
a16   = attack du16
r16   = rest du16


sb_tap :: Bar
sb_tap  = bar +- r16 +- a8 +- a16 +- r16 +- a8  +- a16 
              +- r16 +- a8 +- a16 +- r16 +- a16 +- a8

-- sb_foot = bar +- r4  +- a8 +- a8 +- <TIE> +- a4  +- a8' +- a16
sb_foot = bar +- r4  +- a8 +- a4' +- a8' +- a16

samba_baiao = notelist ->- (transpose (const a5) sb_tap) -\- sb_foot

demo00 = ppNoteList $ samba_baiao
demo01 = ppNoteList $ remeter (2%4) 0 samba_baiao
demo02 = printOM $ sofar $ remeter (2%4) 0 samba_baiao
  where sofar = buildOM . labelOverlays . labelBars
  
demo03 = noteListToLines (4%4) samba_baiao




samba_baiao_sys = mkSystem "samba_baiao" $ remeter (2%4) 0 samba_baiao

main :: IO ()
main = writeMidi "./out/samba_baiao.mid" samba_midi
  where
    samba_midi = generateMidi (4%4) samba_baiao

    