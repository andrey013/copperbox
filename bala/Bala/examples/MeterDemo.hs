
-- ghci ...
-- :set -i../../HNotate:../../Bala

module MeterDemo where

import Bala.Base.Structural
import Bala.Base.Printing

import Bala.Base hiding (a4, duration, Chord)


import qualified HNotate as H

import HNotate.Fits


import qualified Data.Foldable as F
import Data.List hiding (transpose, null)
import Data.Ratio
import Data.Sequence
import Prelude hiding (null)


stranspose_test = stranspose $ fromList (map fromList [[1,2,3],[1,2,3]]) 

fit_test1 :: Fit Int
fit_test1 = fits 10 0

-- use 0 as an indicator of hyphenation
mk_section :: [Int] -> Seq (Seq Int)
mk_section xs = asectionHy (|>0) (fromList xs) 0 4

section_test = let xs  = [1..12]
                   sse = mk_section xs in
  do { print sse; putStrLn ""; print (sumSections sse == sumMeasure xs) }
      


fit_test :: Fit (Seq Int)
fit_test = fitsSeq (fromList [1..5]) 10

fit_test' :: Fit [Int]
fit_test' = fitsList [1..9] 16


attack d = note c4 d

a4    = attack du4
r4    = rest du4
a8    = attack du8
r8    = rest du8
a8'   = attack (dotn 1 du8)
a16   = attack du16
r16   = rest du16


sb_tap :: Bar
sb_tap  = bar +- r16 +- a8 +- a16 +- r16 +- a8  +- a16 
              +- r16 +- a8 +- a16 +- r16 +- a16 +- a8

sb_foot = bar +- r4  +- a8 +- a8  +- a4  +- a8' +- a16

samba_baiao = notelist ->- (transpose (const a5) sb_tap) -\- sb_foot

demo00 = ppNoteList $ samba_baiao
demo01 = ppNoteList $ remeter (2%4) 0 samba_baiao

samba_baiao_sys = mkSystem "samba_baiao" $ remeter (2%4) 0 samba_baiao

main :: IO ()
main = do 
    H.outputMidi (H.set_current_meter (H.mkMeter 2 4))
                 (H.getEventList "samba_baiao") 
                 samba_baiao_sys 
                 "./out/samba_baiao.mid"


     