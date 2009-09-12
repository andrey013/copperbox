{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

-- ghci ...
-- :set -i../src:../../Mullein/src


module Afoxe where

import Bala.BalaMullein
import Bala.BeatPattern
import Bala.Chord
import Bala.ChordDiagram
import Bala.Duration
import Bala.Interval
import Bala.NamedPitches
import Bala.Pitch hiding ( B )  -- TODO BeatPattern needs B renaming
import Bala.Utils

import Mullein.LilyPond hiding ( Duration, rest, makeChord, Pitch )
import qualified Mullein.LilyPond               as M
import qualified Mullein.NamedElements          as M

import Data.AffineSpace
import Text.PrettyPrint.Leijen hiding ( dot )


import Data.Ratio


instance InterpretRest PDGlyph where
  interpretRest = mkRest

--------------------------------------------------------------------------------
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
c6over9'    :: ChordDiagram
c6over9'    = makeChordDiagram [x,3,2,2,3,x]

a7sharp5'   :: ChordDiagram
a7sharp5'   = makeChordDiagram [5,x,5,6,6,x]

dmin9'      :: ChordDiagram
dmin9'      = makeChordDiagram [x,5,3,5,5,x]

g13'        :: ChordDiagram
g13'        = makeChordDiagram [3,x,3,4,5,x]



--------------------------------------------------------------------------------

afoxe_upper :: [Beat Rational]
afoxe_upper = run1 (2%4) $ patt where
  patt = times 4 $ rest 1 >< beats [2,1] >< rest 1 >< beats [2,1]

afoxe_lower :: [Beat Rational] 
afoxe_lower = rewriteRests $ run1 (2%4) afoxe_lower_patt
  where
   rewriteRests = mapAfter 1 fn where
     fn (R a) = B a
     fn a     = a

afoxe_lower_patt :: BeatPattern
afoxe_lower_patt = times 4 $ rest 4 >< beats [2,2]

chordList :: [Chord]
chordList = [c6over9, a7sharp5, dmin9, g13]

afoxeUBuilder :: [Duration -> PDGlyph]
afoxeUBuilder = nrotate 1 $ ntimes 4 $ map upper3 chordList where
  upper3 = mkChord . chordPitches . noRoot

afoxeLBuilder :: [Duration -> PDGlyph]
afoxeLBuilder = zipWith ($) funs $ nrotate 2 (ntimes 3 chordList) where
  funs = [mv,  tied,fn,fn, tied,fn,mv, tied,fn,fn, fn]
  fn   = mkNote . chordRoot
  mv   = mkNote . (.-^ (makeInterval 5 5)) . chordRoot
  tied = (setTied .) . mkNote . chordRoot


afoxeU :: [PDGlyph]
afoxeU = zipInterp afoxeUBuilder afoxe_upper

afoxeL :: [PDGlyph]
afoxeL = zipInterp afoxeLBuilder afoxe_lower 

demo1 :: Doc
demo1 =  version "2.12.2" 
     <$$> chordListDoc
     <$$> variableDef "afoxe"  
           (relative M.middle_c (key M.c_nat "major" <$> time 2 4 <$> tune))
     <$$> variableDef "afoxeChords" (nestBraces fdiags)
     <$$> afoxeTabChordsDef
     <$$> afoxeTabBassDef
     <$$> book (score (staffGroupTemplate <$> layout <$> midi))
  where
    tune    = simpleOutput $ renderPhrase 
                           $ rewritePitch M.middle_c 
                           $ rewriteDuration xs

    fdiags  = simpleOutput $ renderPhrase 
                           $ rewriteDuration 
                           $ phrase two4Tm chordContext

    xs      = overlayPhrases (phrase two4Tm afoxeU) (phrase two4Tm afoxeL)

two4Tm :: MeterPattern
two4Tm  = makeMeterPattern 2 4

{-
    \new StaffGroup << 
      \set StaffGroup.systemStartDelimiter = #'SystemStartSquare
      
      \new Staff 
          << \context Voice = "upper" \afoxeChords 
             \context Voice = "lower" \afoxe 
          >> 
      \new TabStaff {
        \override TabStaff.Stem #'transparent = ##t %% Makes stems transparent
        \override TabStaff.Beam #'transparent = ##t %% Makes beams transparent

          << \context TabVoice = "upper" \afoxeTabChords 
             \context TabVoice = "lower" \afoxeTabBass 
          >>  
      }
    >>


-}

staffGroupTemplate :: Doc
staffGroupTemplate = newStaffGroup $ simultaneous [noteStaff, tabStaff]
  where 
    noteStaff = newStaff $ simultaneous [v1,v2]
    tabStaff  = newTabStaff $ nestBraces (simultaneous [overrides,tv1,tv2])
    v1        = contextVoice "upper" (command "afoxeChords")
    v2        = contextVoice "upper" (command "afoxe")
    tv1       = contextTabVoice "upper" (command "afoxeTabChords")
    tv2       = contextTabVoice "upper" (command "afoxeTabBass")

overrides :: Doc
overrides = vsep $ map text [o1,o2] where
  o1 = "\\override TabStaff.Stem #'transparent = ##t %% Makes stems transparent"
  o2 = "\\override TabStaff.Beam #'transparent = ##t %% Makes beams transparent"


chordListDoc :: Doc
chordListDoc = vsep $ 
   map mkChordDoc [ ('W', "c6/9",  c6over9')
                  , ('X', "a7#5",  a7sharp5')
                  , ('Y', "Dmin9", dmin9')
                  , ('Z', "G13",   g13')]
  where
   mkChordDoc (c,name,ch) = 
           comment name 
       <$> variableDef ("chord" ++[c]) 
                       (markup (fretDiagram $ standardMarkup ch))
       <$> text ""

chordContext :: [SpacerGlyph]
chordContext = map fn ["chordW", "chordX", "chordY", "chordZ"]
  where
    fn s =  markupAboveSpacer (command s) (toDuration $ 2%4)
                
output1 :: IO ()
output1 =  runLilyPond "afoxe.ly"  demo1


tabChords :: [TabGlyph]
tabChords = [ sp16, tabC M.en, tabC M.sn, sp16, tabC M.en, tabA M.sn
            , sp16, tabA M.en, tabA M.sn, sp16, tabA M.en, tabD M.sn
            , sp16, tabD M.en, tabD M.sn, sp16, tabD M.en, tabG M.sn
            , sp16, tabG M.en, tabG M.sn, sp16, tabG M.en, tabC M.sn
            ]
  where
    sp16  = makeSpacer M.sn
    tabC  = tabChord c6over9 
    tabA  = tabChord a7sharp5 
    tabD  = tabChord dmin9
    tabG  = tabChord g13

tabChord ::  Chord -> M.Duration -> TabGlyph  
tabChord ch d = M.makeChord (map toPitch $ chordPitches $ noRoot ch) 
                            d $ [4,3,2::M.StringNumber]


tabBass :: [TabGlyph]
tabBass = [ makeSpacer M.qn, tNote g4 M.en 6, setTied $ tNote a4 M.en 6
          , tNote a4 M.qn 6, tNote a4 M.en 6, setTied $ tNote d5 M.en 5
          , tNote d5 M.qn 5, tNote a4 M.en 6, setTied $ tNote g4 M.en 6
          , tNote g4 M.qn 6, tNote g4 M.en 6, tNote c5 M.en 5]
  where
    tNote :: Pitch -> M.Duration -> M.StringNumber -> TabGlyph
    tNote p d n = M.makeNote (toPitch p) d n
    

afoxeTabChordsDef :: Doc
afoxeTabChordsDef = variableDef "afoxeTabChords" $
    nestBraces (    key M.c_nat "major" 
                <$> time 2 4
                <$> voiceOne
                <$> chords )
  where
    chords = simpleOutput $ renderPhrase 
                          $ rewriteDuration 
                          $ rewritePitchAbs 5
                          $ phrase' (sum two4Tm) tabChords

afoxeTabBassDef :: Doc
afoxeTabBassDef = variableDef "afoxeTabBass" $
    nestBraces (    key M.c_nat "major" 
                <$> time 2 4
                <$> voiceTwo
                <$> basspart )
  where
    basspart = simpleOutput $ renderPhrase 
                          $ rewriteDuration 
                          $ rewritePitchAbs 5
                          $ phrase' (sum two4Tm) tabBass