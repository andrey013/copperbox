

module Afoxe where

import Bala.BalaMullein
import Bala.Chord
import Bala.ChordDiagram
import Bala.Interval
import Bala.NamedPitches
import Bala.Pitch 

import Mullein.Abc ( abcSimple )
import Mullein.LilyPond ( lilyPondSimple )
import qualified Mullein.LilyPond as M

import Data.List ( sort )
import Data.Ratio

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
mulleinChord c = M.Chord (map toPitch $ chordPitches c) M.hn False

guitarChord :: ChordDiagram -> M.PDGlyph
guitarChord cd = M.Chord xs M.hn False where
    xs = sort $ map toPitch $ pitchContent standardTuning cd

demo1 = abcRun $ abcSimple $ tempScore
-- demo2 = lilyPondRun $ lilyPondSimple $ tempScore
demo2 = lilyPondRun $ lilyPondSimple $ zipWith ($) pitchMaterial (replicate 20 $ 1%4)

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