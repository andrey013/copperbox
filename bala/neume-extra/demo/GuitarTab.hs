{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wall #-}


module GuitarTab where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.Pitch
import Neume.Core.Syntax
import Neume.Core.Utils.Pretty 

import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondScoreOutput
import Neume.Extra.NamedElements
import Neume.Extra.ScoreSyntax
import Neume.Extra.Tab

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd



main :: IO ()
main =  
  writeDoc "guitar_tab.ly"      ly_score        >>
  system   "lilypond guitar_tab.ly"             >>
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
tab_tune = variableDef "tabTune" $ 
             nestBraces (key g_nat "major" <$> time 4 4 <$> tab_score_doc)


note_tune :: Doc
note_tune = variableDef "noteTune"  $
              relative middle_c $ (key g_nat "major"
                                      <$> time 4 4 <$> staff_score_doc)


tab_score_doc :: Doc
tab_score_doc = inlineScore barNumber 1 tab_score_ly

staff_score_doc :: Doc
staff_score_doc = inlineScore barNumber 1 staff_score_ly

staff_score_final :: Score (TLinear :. Z) PhraseImage
staff_score_final = lilyPondImageScore (stdLilyPondAlg middle_c) note_score


tab_score_ly :: Score (TLinear :. Z) PhraseImage
tab_score_ly = lilyPondImageScore tabAlg note_score

staff_score_ly :: Score (TLinear :. Z) PhraseImage
staff_score_ly = lilyPondImageScore (stdLilyPondAlg middle_c) note_score


note_score :: Score (TLinear :. Z) (Full (TabGlyph Duration))
note_score = fmap (makeFull cfg . simpleNoteList) $
    Linear gmajor_notes $ Nil
  where
    cfg = bracketConfig [1%2, 1%2]


simpleNoteList :: [e] -> SimpleNoteList e
simpleNoteList = NoteList . map Item

rap :: a -> (a -> b) -> b
rap a f = f a


gmajor_notes :: [TabGlyph Duration]
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

