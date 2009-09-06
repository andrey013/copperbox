{-# OPTIONS -Wall #-}

-- ghci ...
-- :set -i../src:../../Mullein/src


module RhythmExample where


import Bala.BalaMullein
import qualified Bala.NamedPitches as B
import Bala.RhythmPattern

import Mullein.LilyPond hiding ( Pulse, rest, a, b )

import Data.Stream ( Stream )
import qualified Data.Stream              as S
import Data.Stream.Hinze.Stream ( (<:) )
import qualified Data.Stream.Hinze.Stream as HS
import Text.PrettyPrint.Leijen hiding ( dot )

import Data.Ratio


afoxe_upper, afoxe_lower :: SubsetPattern
afoxe_upper = repeatPattern 2 $ makeSubsetPattern 8 [2,4,6,8]
afoxe_lower = repeatPattern 2 $ makeSubsetPattern 8 [5,7]

afoxe_upper_durs :: Stream Rational
afoxe_upper_durs = anarewrite 1 funs (pulse afoxe_upper) where
  funs = [wrap, \a -> let (x,y) = split2 (1,1) a in [x,y]] 

afoxe_lower_durs :: Stream Rational
afoxe_lower_durs = anarewrite 1 funs (pulse afoxe_lower) where
  funs = [wrap, \a -> let (x,y) = split2 (1,2) a in [x,y]]


wrap :: a -> [a]
wrap a = [a] 

afoxeUs :: Stream PDGlyph
afoxeUs = S.zipWith ($) funs afoxe_upper_durs where
  funs = S.cycle [mkRest, mkNote B.c6, mkNote B.c6]

afoxeLs :: Stream PDGlyph
afoxeLs = S.zipWith ($) funs afoxe_lower_durs where
  funs = mkRest <: S.cycle [mkNote B.c5, setTied . mkNote B.c5, mkNote B.c5]


split2 :: (Integer,Integer) -> Rational -> (Rational,Rational)
split2 (a,b) r = ( r * (a%z), r *(b%z)) where z = a+b

afoxeU, afoxeL :: [PDGlyph]
afoxeU = extractBars 0 (2%4) 4 afoxeUs
afoxeL = extractBars 0 (2%4) 4 afoxeLs
 


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
z_lower :: [Rational]
z_lower = S.take 20 $ pulse afoxe_lower

z_upper :: [Rational]
z_upper = S.take 10 $ pulse afoxe_upper 



-- z2 = S.take 10 $ metricalPartition 1 $ pulse afoxe_lower
-- z2' = S.take 10 $ metricalPartition' 1 $ pulse afoxe_lower



