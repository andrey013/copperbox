
-- ghci ...
-- :set -i../src:../../Mullein/src


module RhythmExample where

import Bala.RhythmPattern

import Mullein.LilyPond hiding ( Pulse )

import Data.Stream ( Stream )
import qualified Data.Stream              as WS
import Data.Stream.Hinze.Stream ( (<:) )
import qualified Data.Stream.Hinze.Stream as HS
import Text.PrettyPrint.Leijen

import Data.Ratio
import qualified Data.Set                 as Set

afoxe_upper = makeSubsetPattern 8 [2,4,6,8]
afoxe_lower = makeSubsetPattern 8 [5,7]

-- Hmmm, we're not actually using the pulsation pattern here!

afoxeUs :: Stream PDGlyph
afoxeUs = unwind phi 0 $ pulse afoxe_upper where
  phi _ 0 = (Left snr, 1)
  phi _ 1 = (Left (c 6 en), 2)
  phi _ 2 = (Right [c 6 sn, snr], 1)

-- Mullien - puts tie at start of new bar rather than end of 
-- old bar...

afoxeLs :: Stream PDGlyph
afoxeLs = unwind phi 0 $ pulse afoxe_lower where
  phi _ 0 = (Left qnr, 1)
  phi _ 1 = (Left (c 5 en), 2)
  phi _ 2 = (Right [c 5 en, {- Tie, -} c 5 qn], 1)

afoxeU = WS.take (4*6) afoxeUs
afoxeL = WS.take (3+3*4) afoxeLs
 


demo1 :: Doc
demo1 =  version "2.12.2" 
     <$> score (relative middle_c $ key c_nat "major" <$> time 2 4 <$> tune)
  where
    tune = simpleOutput $ renderPhrase $ rewritePitch middle_c $ rewriteDuration xs
    xs   = overlayPhrases (phrase twoFourTime afoxeU) (phrase twoFourTime afoxeL)


output1 :: IO ()
output1 =  writeDoc "afoxe.ly"  demo1


twoFourTime :: MeterPattern
twoFourTime = makeMeterPattern 2 4



-- rest, note, note, note, note

z1 = WS.take 10 $ fmap (%16) $ pulse afoxe_upper 


z2 = WS.take 10 $ metricalPartition 8 $ pulse afoxe_lower
z2' = WS.take 10 $ metricalPartition' 8 $ pulse afoxe_lower

-- z3 = WS.take 20 $ pass 1 [idS, split, idS] () $ pulse afoxe_upper



