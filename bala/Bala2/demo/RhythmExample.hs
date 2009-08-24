
-- ghci ...
-- :set -i../src:../../Mullein/src


module RhythmExample where

import Bala.RhythmPattern

import Mullein.LilyPond hiding ( Pulse )

import qualified Data.Stream              as WS
import qualified Data.Stream.Hinze.Stream as HS
import Text.PrettyPrint.Leijen


import qualified Data.Set                 as Set


demo0 = showBox dus

dus :: RhythmPattern
dus = makeRhythmPattern s 1 where
  s = [N 2,N 2,R 1,N 2,N 1, R 1,N 2,N 1,R 2,N 2]

afoxe_upper_rhythm :: RhythmPattern
afoxe_upper_rhythm = makeRhythmPattern s 4 where
  s =  [R 1, N 2,N 1]


-- Not quite right - first bar is a rest subsequent bars are 
-- tied...
afoxe_lower_rhythm :: RhythmPattern
afoxe_lower_rhythm = makeRhythmPattern s 4 where
  s =  [R 4, N 2,N 2]



afoxeU :: [PDGlyph]
afoxeU = fmap fn $ takeNBars 4 8 $ toStream afoxe_upper_rhythm
  where
   fn (R 1) = snr
   fn (N 1) = c 6 sn
   fn (N 2) = c 6 en


afoxeL :: [PDGlyph]
afoxeL = fmap fn $ takeNBars 4 8 $ toStream afoxe_lower_rhythm
  where
   fn (R 4) = qnr
   fn (N 2) = c 5 en


demo1 :: Doc
demo1 =  version "2.12.2" 
     <$> score (relative middle_c $ key c_nat "major" <$> time 2 4 <$> tune)
  where
    tune = simpleOutput $ renderPhrase $ rewritePitch middle_c $ rewriteDuration xs
    xs   = overlayPhrases (phrase twoFourTime afoxeU) (phrase twoFourTime afoxeL)


output1 :: IO ()
output1 = do 
  writeDoc "afoxe.ly"  demo1


twoFourTime :: MeterPattern
twoFourTime = makeMeterPattern 2 4

