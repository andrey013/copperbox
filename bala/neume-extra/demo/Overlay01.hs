{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wall #-}

module Overlay1 where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.Pitch
import Neume.Core.Metrical
import Neume.Core.SpellingMap
import Neume.Core.Syntax
import Neume.Core.Utils.Pretty 

import qualified Neume.Extra.AbcDoc           as ABC
import qualified Neume.Extra.AbcScoreOutput   as ABC 
import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondScoreOutput
import Neume.Extra.NamedElements
import Neume.Extra.ScoreSyntax

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd



main :: IO ()
main =
  writeDoc "overlay01.ly"      ly_score                     >>
  writeDoc "overlay01_abc.abc" abc_score                    >>
  system   "lilypond overlay01.ly"                          >>
  system   "abcm2ps overlay01_abc.abc -O overlay01_abc.ps"  >>
  return ()



ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> music_up
        <$> music_down
        <$> global_def
        <$> book (scoreExpr (newStaff $ 
                               nestBraces (variableUse "global" <$> ovs)))
  where
    ovs        = overlay $ map variableUse [ "musicUp", "musicDown" ]
    global_def = variableDef "global" (nestBraces $ 
                                        key c_nat "major" <$> time 4 4)



abc_score :: Doc
abc_score =  ABC.tunenum        1 
         <$> ABC.title          "Overlays"
         <$> ABC.meter          "4/4"
         <$> ABC.key            "Cmaj"
         <$> ABC.unitDuration   dEighth
         <$> abc_tune


music_up :: Doc
music_up = variableDef "musicUp" $ relative middle_c (stemUp <$> udoc)
  where
    udoc = inlineScore barNumber 1 ly_upper

music_down :: Doc
music_down = variableDef "musicDown" $ relative middle_c (stemDown <$> ddoc)
  where
    ddoc = inlineScore barNumber 1 ly_lower


ly_upper :: Score (TRepAlt :. Z) PhraseImage
ly_upper = lilyPondImageScore (stdLilyPondAlg middle_c) upper_score

ly_lower :: Score (TRepAlt :. Z) PhraseImage
ly_lower = lilyPondImageScore (stdLilyPondAlg middle_c) lower_score



abc_tune :: Doc
abc_tune = ABC.inlineScore ABC.barNumber (ABC.lineWidths [5,4]) 1 both
  where
    both = ABC.overlay2 abc_upper abc_lower

abc_upper :: Score (TRepAlt :. Z) PhraseImage
abc_upper = ABC.abcImageScore (ABC.stdAbcAlg c_major (1%8)) upper_score

abc_lower :: Score (TRepAlt :. Z) PhraseImage
abc_lower = ABC.abcImageScore (ABC.stdAbcAlg c_major (1%8)) lower_score


mkScore :: (BeamExtremity e, DMeasure e) => [e] -> Full e
mkScore = makeFull cfg . simpleNoteList
  where
    cfg = bracketConfig [1%2, 1%2]


upper_score :: Score (TRepAlt :. Z) (Full (Glyph () Pitch Duration))
upper_score = fmap mkScore $ 
    RepAlt ubars1'3 [ ubar4A, ubar5B ] $ Nil

lower_score :: Score (TRepAlt :. Z) (Full (Glyph () Pitch Duration))
lower_score = fmap mkScore $ 
    RepAlt lbars1'3 [ lbar4A, lbar5B ] $ Nil


c_major   :: AbcSpellingMap
c_major   = makeAbcSpellingMap 0


simpleNoteList :: [e] -> SimpleNoteList e
simpleNoteList = NoteList . map Item

rap :: a -> (a -> b) -> b
rap a f = f a

ubars1'3 :: [Glyph () Pitch Duration]
ubars1'3 =  
  [ a_ 4 `rap` en, c_ 5 `rap` en, c_ 5 `rap` en, a_ 4 `rap` en
  , c_ 5 `rap` qn, a_ 4 `rap` qn
  
  -- bar 2
  , c_ 5 `rap` en, a_ 4 `rap` en, c_ 5 `rap` en, a_ 4 `rap` en
  , c_ 5 `rap` qn, a_ 4 `rap` qn
  
  -- bar 3
  , a_ 4 `rap` en, c_ 5 `rap` en, c_ 5 `rap` en, a_ 4 `rap` en
  , c_ 5 `rap` qn, a_ 4 `rap` qn
  ]

ubar4A :: [Glyph () Pitch Duration]
ubar4A =
  [ a_ 4 `rap` en, c_ 5 `rap` en, c_ 5 `rap` en, a_ 4 `rap` en
  , a_ 4 `rap` hn
  ]


ubar5B :: [Glyph () Pitch Duration]
ubar5B =
  [ a_ 4 `rap` wn ]


lbars1'3 :: [Glyph () Pitch Duration]
lbars1'3 =  
  [ c_ 4 `rap` hn, c_ 4 `rap` hn
  
  -- bar 2
  , c_ 4 `rap` hn, c_ 4 `rap` hn
  
  -- bar 3
  , c_ 4 `rap` hn, c_ 4 `rap` hn
  ]

lbar4A :: [Glyph () Pitch Duration]
lbar4A = 
  [ c_ 4 `rap` qn, c_ 4 `rap` qn, c_ 4 `rap` hn ]

lbar5B :: [Glyph () Pitch Duration]
lbar5B = 
  [ c_ 4 `rap` wn ]


