{-# LANGUAGE TypeOperators              #-}

module GuitarTab where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondBasic
import Neume.Core.LilyPondFormat
import Neume.Core.LilyPondOutput
import Neume.Core.Pitch
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
import Neume.Core.Utils


import Neume.Extra.Extended
import Neume.Extra.FretDiagrams
import Neume.Extra.LilyPondDoc
import Neume.Extra.NamedElements

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd


main :: IO ()
main = do 
  writeDoc "guitar_tab.ly"      ly_score
  system   "lilypond guitar_tab.ly"
  return ()



ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> empty
        <$> note_tune
        <$> tab_tune
        <$> scoreExpr (simultaneous [tune_staff, tab_staff ])
  where
    tune_staff = newStaff $ 
                   nestBraces (clef "treble" <$> variableUse "noteTune")

    tab_staff  = newTabStaff $
                   nestBraces (stemDown <$> variableUse "tabTune")

tab_tune :: Doc
tab_tune = variableDef "tabTune"  
              $ nestBraces (key g_nat "major" <$> time 4 4 <$> tune1)
  where
    tune1    = renderLyAbsolute ofmt rwspec note_score
    
    ofmt     = Ly_std_format_config       barNumber
    rwspec   = Ly_absolute_rewrite_config (-4) [1%2, 1%2] snum

    snum a d = d <> ppStringNumber a



note_tune :: Doc
note_tune = variableDef "noteTune"  
              $ relative middle_c $ (key g_nat "major"
                                      <$> time 4 4 <$> tune1)
  where
    tune1    = renderLyRelative ofmt rwspec note_score
    
    ofmt     = Ly_std_format_config       barNumber
    rwspec   = Ly_relative_rewrite_config middle_c [1%2, 1%2] strip


note_score :: Score (TLinear :. Z) (NoteList TabGlyph)
note_score = fmap simpleNoteList $
    Linear ("notes", gmajor_notes) $ Nil


gmajor_notes :: [TabGlyph]
gmajor_notes =  
  [ g_  3 `rap` onstring 6 `rap` en 
  , a_  3 `rap` onstring 6 `rap` en
  , b_  3 `rap` onstring 6 `rap` en
  , c_  4 `rap` onstring 6 `rap` en 
  , d_  4 `rap` onstring 5 `rap` en
  , e_  4 `rap` onstring 5 `rap` en
  , fs_ 4 `rap` onstring 4 `rap` en
  , g_  4 `rap` onstring 4 `rap` en
  
  -- bar 2
  , a_  4 `rap` onstring 4 `rap` en
  , b_  4 `rap` onstring 3 `rap` en
  , c_  5 `rap` onstring 3 `rap` en
  , d_  5 `rap` onstring 3 `rap` en 
  , e_  5 `rap` onstring 2 `rap` en
  , fs_ 5 `rap` onstring 2 `rap` en
  , g_  5 `rap` onstring 2 `rap` qn

  ]

onstring :: Int -> (Note a pch) -> Note StringNumber pch
onstring i (Note _ p) = Note (stringNumber i) p

-- oC :: Octave -> Duration 
-- oC oct d fn = fn C oct d
 
vtrf :: (a -> b) -> (b -> c) -> a -> c
vtrf t f x = f (t x)


