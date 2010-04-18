{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wall #-}

module Overlay1 where

import qualified Neume.Core.AbcFormat        as ABC
import Neume.Core.Duration
import Neume.Core.LilyPondFormat
import Neume.Core.Pitch
import Neume.Core.SpellingMap
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
import Neume.Core.Utils

import qualified Neume.Extra.AbcDoc          as ABC
import Neume.Extra.LilyPondDoc
import Neume.Extra.NamedElements

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd


main :: IO ()
main =
  writeDoc "overlay1.ly"      ly_score                          >>
  writeDoc "overlay1_abc.abc" abc_score                         >>
  system   "lilypond overlay1.ly"                               >>
  system   "abcm2ps overlay1_abc.abc -O overlay1_abc.ps"        >>
  return ()



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


abc_score :: Doc
abc_score =  ABC.tunenum        1 
         <$> ABC.title          "Overlays"
         <$> ABC.meter          "4/4"
         <$> ABC.key            "Cmaj"
         <$> ABC.unitDuration   dEighth
         <$> tune1
  where
    tune1   = ABC.renderABC_overlay2 ofmt rwspec rwspec upper_score lower_score
    
    ofmt    = ABC.ABC_std_format_config  [5,4,4,4] ABC.barNumber
    rwspec  = ABC.ABC_std_rewrite_config c_major (1%8) four_four_time



upper_score :: Score (TRepAlt :. Z) (NoteList StdGlyph)
upper_score = fmap simpleNoteList $ 
    RepAlt ("aU", ubars1'3) [ ("aUA", ubar4A), ("aUB", ubar5B) ] $ Nil

lower_score :: Score (TRepAlt :. Z) (NoteList StdGlyph)
lower_score = fmap simpleNoteList $ 
    RepAlt ("aL", lbars1'3) [ ("aLA", lbar4A), ("aLB", lbar5B) ] $ Nil


c_major   :: SpellingMap
c_major   = makeSpellingMap 0


ubars1'3 :: [StdGlyph]
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

ubar4A :: [StdGlyph]
ubar4A =
  [ a_ 4 `rap` en, c_ 5 `rap` en, c_ 5 `rap` en, a_ 4 `rap` en
  , a_ 4 `rap` hn
  ]


ubar5B :: [StdGlyph]
ubar5B =
  [ a_ 4 `rap` wn ]


lbars1'3 :: [StdGlyph]
lbars1'3 =  
  [ c_ 4 `rap` hn, c_ 4 `rap` hn
  
  -- bar 2
  , c_ 4 `rap` hn, c_ 4 `rap` hn
  
  -- bar 3
  , c_ 4 `rap` hn, c_ 4 `rap` hn
  ]

lbar4A :: [StdGlyph]
lbar4A = 
  [ c_ 4 `rap` qn, c_ 4 `rap` qn, c_ 4 `rap` hn ]

lbar5B :: [StdGlyph]
lbar5B = 
  [ c_ 4 `rap` wn ]


