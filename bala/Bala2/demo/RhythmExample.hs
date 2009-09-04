
-- ghci ...
-- :set -i../src:../../Mullein/src


module RhythmExample where

import Bala.RhythmPattern

import Mullein.LilyPond hiding ( Pulse, rest )

import Data.Stream ( Stream )
import qualified Data.Stream              as WS
import Data.Stream.Hinze.Stream ( (<:) )
import qualified Data.Stream.Hinze.Stream as HS
import Text.PrettyPrint.Leijen hiding ( dot )

import Data.Ratio
import qualified Data.Set                 as Set

afoxe_upper = repeatPattern 2 $ makeSubsetPattern 8 [2,4,6,8]
afoxe_lower = repeatPattern 2 $ makeSubsetPattern 8 [5,7]



afoxeUs :: Stream PDGlyph
afoxeUs = unwind phi 0 $ pulse afoxe_upper where
  phi a 0 = (Left $ rest a, 1)
  phi a 1 = (Left (c 6 $ balaDuration a), 2)
  phi a 2 = (Right $ c16r16 a, 1)


c16r16 :: Rational -> [PDGlyph]
c16r16 d = [c 6 $ balaDuration d1, rest d2]
  where
    (d1,d2) = split2 (1,1) d


afoxeLs :: Stream PDGlyph
afoxeLs = unwind phi 0 $ pulse afoxe_lower where
  phi a 0 = (Left $ rest a, 1)
  phi a 1 = (Left (c 5 $ balaDuration a), 2)
  phi a 2 = (Right $ tied8'4 a, 1)


tied8'4 :: Rational -> [PDGlyph]
tied8'4 d = [setTied $ c 5 d1, c 5 d2]
  where
    (d1,d2) = fork (balaDuration,balaDuration) $ split2 (1,2) d

split2 :: (Integer,Integer) -> Rational -> (Rational,Rational)
split2 (a,b) r = ( r * (a%z), r *(b%z)) where z = a+b

-- note :: Pitch -> Octave -> Rational -> PDGlyph
-- note p o = makeNote p o . balaDuration

rest :: Rational -> PDGlyph
rest = makeRest . balaDuration

fork (f,g) (a,b) = (f a, g b)

balaDuration :: Rational -> Duration
balaDuration  = either restErr id . rationalToDuration 
  where
    restErr :: ConversionError -> Duration
    restErr e = error $ "balaDuration - cannot convert " 
                      ++ show (getBadRational e)



afoxeU = WS.take (4*6) afoxeUs
afoxeL = WS.take (4*3) afoxeLs
 


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



-- rest, note, note, note, no
z_lower = WS.take 20 $ pulse afoxe_lower

z_upper = WS.take 10 $ pulse afoxe_upper 



-- z2 = WS.take 10 $ metricalPartition 1 $ pulse afoxe_lower
-- z2' = WS.take 10 $ metricalPartition' 1 $ pulse afoxe_lower



