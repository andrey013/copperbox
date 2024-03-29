{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}



module Afoxe where

import Bala.BeatPatternOld
import Bala.Chord
import Bala.ChordDiagram
import Bala.Duration
import Bala.Interval
import Bala.Mullein
import Bala.NamedPitches
import Bala.Pitch
import Bala.Structural hiding ( strings )
import Bala.Utils

import Mullein.LilyPond hiding ( Duration, rest, makeChord, Pitch )
import qualified Mullein.LilyPond               as M
import qualified Mullein.NamedElements          as M

import Data.AffineSpace
import Text.PrettyPrint.Leijen hiding ( dot )


import Data.Ratio



instance InterpretRest PDGlyph where
  interpretRest = mkRest

instance InterpretRest TabGlyph where
  interpretRest = makeSpacer . toDuration

--------------------------------------------------------------------------------
-- chords

c6over9 :: Chord
c6over9 = makeChord c5 [perfect1, major3, major6, major9]

c6over9' :: FretDiagramDef
c6over9' = ("ChordW", "C6/9", makeChordDiagram [x,3,2,2,3,x])

a7sharp5 :: Chord
a7sharp5 = makeChord a4 [perfect1, minor7, major3 # addOctave, minor6 # addOctave]

a7sharp5' :: FretDiagramDef
a7sharp5' = ("chordX", "A7#5", makeChordDiagram [5,x,5,6,6,x])

dmin9 :: Chord
dmin9 = minor d5 # no5 # min9

dmin9' :: FretDiagramDef
dmin9' = ("ChordY", "Dmin9", makeChordDiagram [x,5,3,5,5,x])

g13 :: Chord
g13 =  makeChord g4 [perfect1, minor7, major3 # addOctave, major6 # addOctave]

g13' :: FretDiagramDef
g13' = ("ChordZ", "G13", makeChordDiagram [3,x,3,4,5,x])



--------------------------------------------------------------------------------

chordList :: [Chord]
chordList = [c6over9, a7sharp5, dmin9, g13]

fretDiags :: [FretDiagramDef]
fretDiags = [c6over9', a7sharp5', dmin9', g13']


expandedChordPattern :: [Chord]
expandedChordPattern = ntimes 4 chordList <<& 1


afoxe_upper :: [Beat Rational ()]
afoxe_upper = run1 (2%4) $ patt where
  patt = times 4 $ rest 1 >< beats [2,1] >< rest 1 >< beats [2,1]

afoxe_lower :: [Beat Rational ()] 
afoxe_lower = rewriteRests $ run1 (2%4) afoxe_lower_patt
  where
    rewriteRests = mapAfter 1 fn where
      fn (Rb a) = Nb a ()
      fn a      = a

    afoxe_lower_patt :: BeatPattern
    afoxe_lower_patt = times 4 $ rest 4 >< beats [2,2]


bassPattern :: [Duration -> PDGlyph]
bassPattern = zipWith ($) funs chs
  where
    chs  = (drop 1 $ ntimes 3 chordList) <<& 1
    funs = [mv,  tied,fn,fn, tied,fn,mv, tied,fn,fn, fn]
    fn   = annoZero . mkNote . chordRoot
    mv   = annoZero . mkNote . (.-^ (makeInterval 5 5)) . chordRoot
    tied = (setTied .) . annoZero . mkNote . chordRoot

    annoZero f = f ()

chordVoice :: [PDGlyph]
chordVoice = leftInterp chordVoicefs afoxe_upper

bassVoice :: [PDGlyph]
bassVoice = leftInterp bassPattern afoxe_lower


chordTabVoice :: [TabGlyph]
chordTabVoice = leftInterp chordTabfs afoxe_upper


bassTabVoice :: [TabGlyph]
bassTabVoice = replaceRests $ distAnnos' expand strings 
                            $ leftInterp bassPattern afoxe_lower
  where
    strings    :: [StringNumber]
    strings    = [6,6,6,6, 5,5, 6,6,6,6, 5]

    expand :: StringNumber -> () ->StringNumber
    expand i _ = i

chordVoicefs :: [Duration -> PDGlyph]
chordVoicefs = chs where
  chs = map (mkChord . map aZ . pitchContent . noRoot) expandedChordPattern
  aZ p = (p,())

chordTabfs :: [Duration -> TabGlyph]
chordTabfs = chs where
  chs = map tabChord expandedChordPattern

  tabChord ::  Chord -> Duration -> TabGlyph  
  tabChord ch d = M.makeChord (zip (map toPitch $ pitchContent $ noRoot ch) strs)
                              (toDuration d) 
    where strs = [4,3,2::M.StringNumber]




fretDiagramsDef :: Doc
fretDiagramsDef = fretDiagramDefs fretDiags

fretDiagramsUse :: [SpacerGlyph]
fretDiagramsUse = zipWith mkFDiag fretDiags (repeat (2%4)) where
  mkFDiag ch d = markupAboveSpacer (command $ fst3 ch) (toDuration d)
  fst3 (a,_,_) = a

overrides :: Doc
overrides = vsep [o1,o2] where
  o1 = override "TabStaff.Stem" "transparent" lyTrue
  o2 = override "TabStaff.Beam" "transparent" lyTrue



demo1 :: Doc
demo1 =  version "2.12.2" 
     <^> fretDiagramsDef
     <^> fretDiagramPictures "afoxeChordDiags" 
                             (sum $ meterPattern M.two_four_time) 
                             fretDiagramsUse
     <^> variableDef "afoxeNotes"  
           (relative M.middle_c (key M.c_nat "major" 
                                     <$> time' M.two_four_time 
                                     <$> tune))

     <^> afoxeTabDef

     <^> book (scoreExpr (staffGroupTemplate <$> layout <$> midi))
  where
    tune    = simpleOutput $ renderPhrase lyGlyph
                           $ rewritePitch M.middle_c 
                           $ rewriteDuration xs
    xs      = overlayNoteLists (meterPattern M.two_four_time)
                               [chordVoice, bassVoice]




staffGroupTemplate :: Doc
staffGroupTemplate = newStaffGroup $ simultaneous [noteStaff, tabStaff]
  where 
    noteStaff = newStaff    $ simultaneous [v1,v2]
    tabStaff  = newTabStaff $ nestBraces (simultaneous [overrides,tv1,tv2])
    v1        = contextVoice    "upper" (command "afoxeChordDiags")
    v2        = contextVoice    "lower" (command "afoxeNotes")
    tv1       = contextTabVoice "upper" (command "afoxeTabChords")
    tv2       = contextTabVoice "lower" (command "afoxeTabBass")



afoxeTabDef :: Doc
afoxeTabDef = chordBassTabDef (M.c_nat, "major")
                              M.two_four_time
                              ("afoxeTabChords",chordTabVoice)
                              ("afoxeTabBass",  bassTabVoice)
                              
                
output1 :: IO ()
output1 =  runLilyPond "afoxe.ly"  demo1




