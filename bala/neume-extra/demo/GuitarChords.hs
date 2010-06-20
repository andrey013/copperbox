{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wall #-}


module GuitarChords where

import Neume.Core.Duration
import Neume.Core.Pitch
import Neume.Core.Syntax
import Neume.Core.Utils.Pretty ( writeDoc )

import Neume.Extra.Common
import Neume.Extra.FretDiagrams
import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondScoreOutput
import Neume.Extra.NamedElements
import Neume.Extra.ScoreSyntax

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd



main :: IO ()
main = 
  writeDoc "guitar_chords.ly"      ly_score     >>
  system   "lilypond guitar_chords.ly"          >>
  return ()



ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> chord_defs
        <$> empty
        <$> chord_tune
        <$> empty
        <$> note_tune
        <$> book (scoreExpr (staff_doc <$> layoutExpr empty))
  where
    staff_doc  = newStaff $ simultaneous $
                    [ mkNewVoice "chords" "voiceOne" "chordTune" 
                    , mkNewVoice "notes"  "voiceTwo" "noteTune" ]
 
    mkNewVoice a vname tname = 
      newVoice (equals <+> (dquotes $ text a) 
                       <+> braces (command vname <+> variableUse tname))
chord_defs :: Doc
chord_defs = diagDefsList [ chord_G6, chord_C, chord_D7, chord_G ]

chord_tune :: Doc
chord_tune = variableDef "chordTune" $ nestBraces (time 4 4 <$> tune1)
  where
    tune1    = renderFretDiag ofmt rwcfg chord_score
    ofmt     = Ly_std_format_config barNumber
    rwcfg    = Ly_fret_diag_config  [1%2, 1%2]


note_tune :: Doc
note_tune = variableDef "noteTune"  
              $ relative middle_c $ (key g_nat "major"
                                      <$> time 4 4 <$> tune1)
  where
    tune1    = renderLyRelative ofmt rwspec note_score
    
    ofmt     = Ly_std_format_config       barNumber
    rwspec   = Ly_relative_rewrite_config middle_c [1%2, 1%2] strip


chord_score :: Score (TLinear :. Z) (NoteList FretDiagramGlyph)
chord_score = fmap simpleNoteList $
    Linear ("chords", chord_list) $ Nil

note_score :: Score (TLinear :. Z) (NoteList (Glyph () Pitch Duration))
note_score = fmap simpleNoteList $
    Linear ("notes", arp_notes) $ Nil

diag :: FretDiagram -> Duration -> FretDiagramGlyph
diag = Graphic



chord_list :: [FretDiagramGlyph]
chord_list = [ diag chord_G6 dHalf, diag chord_C dHalf
              , diag chord_D7 dHalf, diag chord_G dHalf ]


chord_G6  :: FretDiagram
chord_G6  = fretDiagram "G6" "chGSix"   [ x_none, x_none, 5,4,5, x_none ]

chord_C   :: FretDiagram 
chord_C   = fretDiagram "C"  "chC"      [ x_none, 3,2, x_none, 1, x_none ]

chord_D7  :: FretDiagram 
chord_D7  = fretDiagram "D7" "chDSeven" [ x_none, 5,4,5,3, x_none ]

chord_G   :: FretDiagram
chord_G   = fretDiagram "G"  "chG"      [ x_none, x_none, 5,4,3, x_none ]

rap :: a -> (a -> b) -> b
rap a f = f a

simpleNoteList :: [e] -> SimpleNoteList e
simpleNoteList = NoteList . map Item


arp_notes :: [Glyph () Pitch Duration]
arp_notes =  
  [ g_ 4 `rap` qn,  b_ 4 `rap` en, e_ 5 `rap` en
  , c_ 4 `rap` qn,  e_ 4 `rap` en, c_ 5 `rap` en
  
  -- bar 2
  , d_ 4 `rap` en, fs_ 4 `rap` en, c_ 5 `rap` en, d_ 5 `rap` en 
  , g_ 4 `rap` qn, b_  4 `rap` en, d_ 5 `rap` en

  ]
