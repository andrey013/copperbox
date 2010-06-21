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
import Neume.Extra.NamedElements
import Neume.Extra.ScoreSyntax

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd



main :: IO ()
main =
--  writeDoc "overlay1.ly"      ly_score                          >>
  writeDoc "overlay1_abc.abc" abc_score                         >>
--  system   "lilypond overlay1.ly"                               >>
  system   "abcm2ps overlay1_abc.abc -O overlay1_abc.ps"        >>
  return ()

{-

ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> para_defs
        <$> global_def
        <$> book (scoreExpr (newStaffGroup (overlay [music]) <$> layout))
                       
  where
    global_def   = variableDef "global" (nestBraces $ key c_nat "major" <$> time 4 4)
    music        = newStaff (variableUse "global" <$> overlay [voice1,voice2])
    voice1       = relative middle_c (stemUp   <$> v1)
    voice2       = relative middle_c (stemDown <$> v2)
    v1           = scoreLy_parallel2 upper_score
    v2           = scoreLy_parallel2 lower_score
    para_defs    = renderLyRelative_parallel2 dWhole ofmt 
                                              rwspec rwspec upper_score lower_score
    
    ofmt         = Ly_std_format_config barNumber
    rwspec       = Ly_relative_rewrite_config middle_c four_four_time strip

-}

abc_score :: Doc
abc_score =  ABC.tunenum        1 
         <$> ABC.title          "Overlays"
         <$> ABC.meter          "4/4"
         <$> ABC.key            "Cmaj"
         <$> ABC.unitDuration   dEighth
         <$> abc_tune

abc_tune :: Doc
abc_tune = ABC.inlineScore ABC.barNumber (ABC.lineWidths [5,4]) 1 both
  where
    both = ABC.overlay2 abc_upper abc_lower

abc_upper = ABC.abcImageScore (ABC.stdAbcAlg c_major (1%8)) upper_score


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


