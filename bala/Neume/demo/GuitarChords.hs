{-# LANGUAGE TypeOperators              #-}

module GuitarChords where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondBasic
import Neume.Core.LilyPondOutput
import Neume.Core.Pitch
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
import Neume.Core.Utils


import Neume.Extra.Extended
import Neume.Extra.FretDiagrams
import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondFormat
import Neume.Extra.NamedElements

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd



main :: IO ()
main = do 
  writeDoc "guitar_chords.ly"      ly_score
  system   "lilypond guitar_chords.ly"
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
    ofmt     = Ly_Std_Format_Config barNumber
    rwcfg    = Ly_Fret_Diag_Config  [1%2, 1%2]


note_tune :: Doc
note_tune = variableDef "noteTune"  
              $ relative middle_c $ (key g_nat "major"
                                      <$> time 4 4 <$> tune1)
  where
    tune1    = renderLyRelative ofmt rwspec note_score
    
    ofmt     = Ly_Std_Format_Config       barNumber
    rwspec   = Ly_Relative_Rewrite_Config middle_c [1%2, 1%2]


chord_score :: Score (TLinear :. Z) (NoteList FretDiagramGlyph)
chord_score = fmap simpleNoteList $
    Linear ("chords", chord_list) $ Nil

note_score :: Score (TLinear :. Z) (NoteList StdGlyph)
note_score = fmap simpleNoteList $
    Linear ("notes", arp_notes) $ Nil

diag :: FretDiagram -> Duration -> FretDiagramGlyph
diag = MGlyph 



chord_list :: [FretDiagramGlyph]
chord_list = [ diag chord_G6 hn, diag chord_C hn
              , diag chord_D7 hn, diag chord_G hn ]


chord_G6  :: FretDiagram
chord_G6  = fretDiagram "G6" "chGSix"   [ x_none, x_none, 5,4,5, x_none ]

chord_C   :: FretDiagram 
chord_C   = fretDiagram "C"  "chC"      [ x_none, 3,2, x_none, 1, x_none ]

chord_D7  :: FretDiagram 
chord_D7  = fretDiagram "D7" "chDSeven" [ x_none, 5,4,5,3, x_none ]

chord_G   :: FretDiagram
chord_G   = fretDiagram "G"  "chG"      [ x_none, x_none, 5,4,3, x_none ]


arp_notes :: [StdGlyph]
arp_notes =  
  [ g 4 qn,  b 4 en, e 5 en
  , c 4 qn,  e 4 en, c 5 en
  
  -- bar 2
  , d 4 en, fs 4 en, c 5 en, d 5 en 
  , g 4 qn, b  4 en, d 5 en

  ]
