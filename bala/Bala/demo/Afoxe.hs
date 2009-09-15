{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
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
import Bala.SequenceManipulation
import Bala.Utils hiding ( ntimes ) -- ntimes to be deleted

import Mullein.LilyPond hiding ( Duration, rest, makeChord, Pitch )
import qualified Mullein.LilyPond               as M
import qualified Mullein.NamedElements          as M

import Data.AffineSpace
import Text.PrettyPrint.Leijen hiding ( dot )


import Data.Ratio


data GuitarChord = GC { 
      getChordName    :: String,
      getChordAlias   :: String, 
      getChord        :: Chord, 
      getFretDiagram  :: ChordDiagram 
    }

instance InterpretRest PDGlyph where
  interpretRest = mkRest

instance InterpretRest TabGlyph where
  interpretRest = makeSpacer . toDuration

instance HasTie (StringNumber -> TabGlyph) where
  setTied f = \i -> setTied (f i)

infixr 5 <^>
(<^>) :: Doc -> Doc -> Doc 
(<^>) a b = a <$$> text "" <$$> b


--------------------------------------------------------------------------------
-- chords

c6over9 :: GuitarChord
c6over9 = GC nm as ch cd where
  nm = "C6/9"
  as = "chordW"
  ch = makeChord c5 [perfect1, major3, major6, major9]
  cd = makeChordDiagram [x,3,2,2,3,x]

a7sharp5 :: GuitarChord
a7sharp5 = GC nm as ch cd where
  nm = "A7#5"
  as = "chordX"
  ch = makeChord a4 [perfect1, minor7, major3 # addOve, minor6 # addOve]
  cd = makeChordDiagram [5,x,5,6,6,x]

dmin9 :: GuitarChord
dmin9 = GC nm as ch cd where
  nm = "Dmin9"
  as = "chordY"
  ch = minor d5 # no5 # min9
  cd = makeChordDiagram [x,5,3,5,5,x]

g13 :: GuitarChord
g13 = GC nm as ch cd where
  nm = "G13"
  as = "chordZ"
  ch = makeChord g4 [perfect1, minor7, major3 # addOve, major6 # addOve]
  cd = makeChordDiagram [3,x,3,4,5,x]



--------------------------------------------------------------------------------

chordList :: [GuitarChord]
chordList = [c6over9, a7sharp5, dmin9, g13]


afoxe_upper :: [Beat Rational]
afoxe_upper = run1 (2%4) $ patt where
  patt = times 4 $ rest 1 >< beats [2,1] >< rest 1 >< beats [2,1]

afoxe_lower :: [Beat Rational] 
afoxe_lower = rewriteRests $ run1 (2%4) afoxe_lower_patt
  where
    rewriteRests = mapAfter 1 fn where
      fn (R a) = N a
      fn a     = a

    afoxe_lower_patt :: BeatPattern
    afoxe_lower_patt = times 4 $ rest 4 >< beats [2,2]

expandedChordPattern :: [GuitarChord]
expandedChordPattern = ntimes 4 chordList <<& 1


bassPattern :: (MakeNote e, HasTie e) => [Duration -> e]
bassPattern = zipWith ($) funs chs
  where
    chs  = map getChord $ ((drop 1 $ ntimes 3 chordList) <<& 1)
    funs = [mv,  tied,fn,fn, tied,fn,mv, tied,fn,fn, fn]
    fn   = mkNote . chordRoot
    mv   = mkNote . (.-^ (makeInterval 5 5)) . chordRoot
    tied = (setTied .) . mkNote . chordRoot



chordVoice :: [PDGlyph]
chordVoice = zipInterp chordVoicefs afoxe_upper

bassVoice :: [PDGlyph]
bassVoice = zipInterp bassPattern afoxe_lower


chordTabVoice :: [TabGlyph]
chordTabVoice = zipInterp chordTabfs afoxe_upper


bassTabVoice :: [TabGlyph]
bassTabVoice = zipInterp (zipWith expand bassPattern strings) afoxe_lower
  where
    strings    :: [StringNumber]
    strings    = [6,6,6,6, 5,5, 6,6,6,6, 5]

expand :: (Duration -> StringNumber -> TabGlyph) 
       -> StringNumber 
       -> (Duration -> TabGlyph)
expand f i = \d -> f d i

chordVoicefs :: [Duration -> PDGlyph]
chordVoicefs = chs where
  chs = map (mkChord . chordPitches . noRoot . getChord) expandedChordPattern


chordTabfs :: [Duration -> TabGlyph]
chordTabfs = chs where
  chs = map (tabChord . getChord) expandedChordPattern

  tabChord ::  Chord -> Duration -> TabGlyph  
  tabChord ch d = M.makeChord (map toPitch $ chordPitches $ noRoot ch) 
                              (toDuration d) $ [4,3,2::M.StringNumber]



two4Tm :: MeterPattern
two4Tm  = makeMeterPattern 2 4




fretDiagramsDef :: [Doc]
fretDiagramsDef = map phi chordList where
  phi (GC name alias _ diag) =  comment name 
                            <$> variableDef alias (chi diag)
  chi = markup . fretDiagram . standardMarkup


fretDiagramsUse :: [SpacerGlyph]
fretDiagramsUse = zipWith mkFDiag chordList (repeat (2%4)) where
  mkFDiag ch d = markupAboveSpacer (command $ getChordAlias ch) (toDuration d)


overrides :: Doc
overrides = vsep $ map text [o1,o2] where
  o1 = "\\override TabStaff.Stem #'transparent = ##t %% Makes stems transparent"
  o2 = "\\override TabStaff.Beam #'transparent = ##t %% Makes beams transparent"



demo1 :: Doc
demo1 =  version "2.12.2" 
     <^> (vsep fretDiagramsDef)
     <^> variableDef "afoxeChordDiags" (nestBraces fdUse)
     <^> variableDef "afoxeNotes"  
           (relative M.middle_c (key M.c_nat "major" <$> time 2 4 <$> tune))

     <^> afoxeTabChordsDef
     <^> afoxeTabBassDef

     <^> book (score (staffGroupTemplate <$> layout <$> midi))
  where
    fdUse   = simpleOutput $ renderPhrase lySpacerGlyph
                           $ rewriteDuration 
                           $ phraseNoPulses (sum two4Tm) fretDiagramsUse

    tune    = simpleOutput $ renderPhrase lyGlyph
                           $ rewritePitch M.middle_c 
                           $ rewriteDuration xs
    xs      = overlayPhrases (phrase two4Tm chordVoice)
                             (phrase two4Tm bassVoice)




staffGroupTemplate :: Doc
staffGroupTemplate = newStaffGroup $ simultaneous [noteStaff, tabStaff]
  where 
    noteStaff = newStaff    $ simultaneous [v1,v2]
    tabStaff  = newTabStaff $ nestBraces (simultaneous [overrides,tv1,tv2])
    v1        = contextVoice    "upper" (command "afoxeChordDiags")
    v2        = contextVoice    "lower" (command "afoxeNotes")
    tv1       = contextTabVoice "upper" (command "afoxeTabChords")
    tv2       = contextTabVoice "lower" (command "afoxeTabBass")



afoxeTabChordsDef :: Doc
afoxeTabChordsDef = variableDef "afoxeTabChords" $
    nestBraces (    key M.c_nat "major" 
                <$> time 2 4
                <$> voiceOne
                <$> chords )
  where
    chords = simpleOutput $ renderPhrase lyTabGlyph
                          $ rewriteDuration 
                          $ rewritePitchAbs (-5)
                          $ phraseNoPulses (sum two4Tm) chordTabVoice


afoxeTabBassDef :: Doc
afoxeTabBassDef = variableDef "afoxeTabBass" $
    nestBraces (    key M.c_nat "major" 
                <$> time 2 4
                <$> voiceTwo
                <$> basspart )
  where
    basspart = simpleOutput $ renderPhrase lyTabGlyph
                            $ rewriteDuration 
                            $ rewritePitchAbs (-5)
                            $ phraseNoPulses (sum two4Tm) bassTabVoice


                
output1 :: IO ()
output1 =  runLilyPond "afoxe.ly"  demo1




