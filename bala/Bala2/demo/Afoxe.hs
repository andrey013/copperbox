

module Afoxe where

import Bala.BalaMullein
import Bala.Chord
import Bala.ChordDiagram
import Bala.Duration
import Bala.Interval
import Bala.NamedPitches
import Bala.Pitch 
import Bala.RhythmPattern

import Mullein.Abc ( abcSimple )
import Mullein.LilyPond hiding ( Pitch, Duration, makeChord )
import qualified Mullein.LilyPond               as M
import qualified Mullein.NamedElements          as M

import Data.AffineSpace

import Data.Stream ( Stream, head, tail )
import qualified Data.Stream                    as S
import Data.Stream.Hinze.Stream ( (<<), (<:) )

import Text.PrettyPrint.Leijen hiding ( dot )


import Data.List ( sort )
import Data.Ratio
import Prelude hiding ( head, tail )

-- Reverse application

infixl 7 #

( # ) :: a -> (a -> b) -> b 
x # f = f x

-- chords

c6over9 :: Chord
c6over9 = makeChord c5 [perfect1, major3, major6, major9]

a7sharp5 :: Chord
a7sharp5 = makeChord a4 [perfect1, minor7, major3 # addOve, minor6 # addOve]

dmin9 :: Chord
dmin9 = minor d5 # no5 # min9

g13 :: Chord
g13 = makeChord g4 [perfect1, minor7, major3 # addOve, major6 # addOve]

-- chord diagrams
c6over9_cd  :: ChordDiagram
c6over9_cd  = makeChordDiagram [x,3,2,2,3,x]

a7sharp5_cd :: ChordDiagram
a7sharp5_cd = makeChordDiagram [5,x,5,6,6,x]

dmin9_cd    :: ChordDiagram
dmin9_cd    = makeChordDiagram [x,5,3,5,5,x]

g13_cd      :: ChordDiagram
g13_cd      = makeChordDiagram [3,x,3,4,5,x]


mulleinChord :: Chord -> M.PDGlyph
mulleinChord c = mkChord (chordPitches c) (1%2)

guitarChord :: ChordDiagram -> M.PDGlyph
guitarChord cd = mkChord xs (1%2) where
    xs = sort $ pitchContent standardTuning cd

demoZ1 = abcRun $ abcSimple $ tempScore
-- demo2 = lilyPondRun $ lilyPondSimple $ tempScore
demoZ2 = lilyPondRun $ lilyPondSimple $ zipWith ($) pitchMaterial (replicate 20 $ 1%4)

-- The /pitch material/ in afoxe exercise
tempScore = xs ++ ys ++ zs
  where 
    xs = map mulleinChord chordList
    ys = map guitarChord guitarChordList
    zs = map (\p -> mkNote p (1%4)) [g4,a4,d5,a4,g4,c5]

chordList = [c6over9, a7sharp5, dmin9, g13]

guitarChordList  = [c6over9_cd, a7sharp5_cd, dmin9_cd, g13_cd]


wandering :: Chord -> Pitch -> [Rational -> M.PDGlyph]
wandering ch p = 
    [mkNote (chordRoot ch), upper3, upper3, upper3, mkNote p, upper3]
  where
    upper3 = mkChord (chordPitches $ noRoot ch)

pitchMaterial :: [Rational -> M.PDGlyph]
pitchMaterial = concat [wandering     c6over9 g4, 
                        wanderingRoot a7sharp5, 
                        wandering     dmin9 a4, 
                        wanderingRoot g13]

  where
    wanderingRoot ch = wandering ch (chordRoot ch)


chordStream :: Stream Chord
chordStream = drop1 $ strm where 
  strm = concat xs << strm 
  xs   = map (replicate 4 . noRoot) chordList


drop1 :: Stream a -> Stream a
drop1 = S.tail

-- wanderin' bass
wanderinStream :: Stream Pitch
wanderinStream = S.drop 2 $ rewrite funs (S.cycle chordList) where
  funs = map (. chordRoot) [ \a -> [a, mv a, mv a], \a -> [a,a,a]
                           , \a -> [a, a, mv a], \a -> [a,a,a] ]
  mv   = (.-^ (makeInterval 5 5))


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


split2 :: (Integer,Integer) -> Rational -> (Rational,Rational)
split2 (a,b) r = ( r * (a%z), r *(b%z)) where z = a+b



afoxeUsF :: Stream (Duration -> PDGlyph)
afoxeUsF = mkStream $ paired chordStream where 
  mkStream s = mkRest <: chord a <: chord b <: mkStream (tail s) where 
               (a,b) = head s
  chord      = mkChord . chordPitches    

afoxeUs :: Stream PDGlyph
afoxeUs = S.zipWith ($) afoxeUsF afoxe_upper_durs

afoxeLsF :: Stream (Duration -> PDGlyph)
afoxeLsF = mkRest <: S.zipWith ($) (S.cycle xs) wanderinStream where
  xs = [mkNote, (setTied .) . mkNote, mkNote]


afoxeLs :: Stream PDGlyph
afoxeLs = S.zipWith ($) afoxeLsF afoxe_lower_durs


paired :: Stream a -> Stream (a,a) 
paired strm = (a,b) <: paired strm'' where
  (a,strm')  = (head strm, tail strm)
  (b,strm'') = (head strm', tail strm')


-- afoxeU, afoxeL :: [PDGlyph]
afoxeU = extractBars 0 (2%4) 4 afoxeUs
afoxeL = extractBars 0 (2%4) 4 afoxeLs


demo1 :: Doc
demo1 =  version "2.12.2" 
     <$> score (relative M.middle_c $ key M.c_nat "major" <$> time 2 4 <$> tune)
  where
    tune = simpleOutput $ renderPhrase 
                        $ rewritePitch M.middle_c 
                        $ rewriteDuration xs
    xs   = overlayPhrases (phrase twoFourTime afoxeU) (phrase twoFourTime afoxeL)


output1 :: IO ()
output1 =  writeDoc "afoxe.ly"  demo1


twoFourTime :: MeterPattern
twoFourTime = makeMeterPattern 2 4


