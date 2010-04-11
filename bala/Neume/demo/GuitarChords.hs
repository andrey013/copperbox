{-# LANGUAGE TypeOperators              #-}

module GuitarChords where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondBasic
import Neume.Core.LilyPondOutput
import Neume.Core.Pitch
import Neume.Core.SyntaxMarkup
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
import Neume.Core.SyntaxStaff
import Neume.Core.Utils


import Neume.Extra.Extended
import Neume.Extra.FretDiagrams
import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondFormat
import Neume.Extra.NamedElements

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd

{-

main :: IO ()
main = do 
  writeDoc "guitar_chords.ly"      ly_score
  system   "lilypond guitar_chords.ly"
  return ()



ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> chordtune


chordtune :: Doc
chordtune = variableDef "chordTune" $ nestBraces tune1
  where
    tune1    = lilypondFretDiagScore mkPhrase strip chord_score
    mkPhrase = lyPhraseFretDiagrams [1%2, 1%2]
-}

chord_score :: Score (TRepeat :. Z) (NoteList FretDiagramGlyph)
chord_score = fmap simpleNoteList $
    Repeat ("chords", diag_notes) $ Nil


diag :: FretDiagram -> Duration -> FretDiagramGlyph
diag = SGlyph 



diag_notes :: [FretDiagramGlyph]
diag_notes = [ diag chord_C6_over_9 hn ]


chord_C6_over_9 :: FretDiagram
chord_C6_over_9 = fretDiagram "C6/9" "chCSixOverNine" [x_none, 3,2,2,3, x_none]