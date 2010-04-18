{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

module Percussion where

import Neume.Core.Duration
import Neume.Core.LilyPondFormat
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
import Neume.Core.Utils
import Neume.Core.Utils.OneList ( fromList )

import Neume.Extra.DrumPitches
import Neume.Extra.LilyPondDoc
import Neume.Extra.Percussion

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd



main :: IO ()
main =
  writeDoc "percussion.ly"      ly_score        >>
  system   "lilypond percussion.ly"             >>
  return ()



ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> drumtune
        <$> scoreExpr (new "DrumStaff" 
                        (with dstyle (simultaneous [variableUse "drumtune"])))
  where
    dstyle = definition "drumStyleTable" (text "#drums-style")


drumtune :: Doc
drumtune = variableDef "drumtune" $ drummode (time 4 4 <$> stemUp <$> tune1 )
  where
    tune1    = renderLyDrums ofmt orw drum_score
        
    ofmt     = Ly_std_format_config       strip
    orw      = Ly_drums_rewrite_config [1%2, 1%2] strip


drum_score :: Score (TRepeat :. Z) (NoteList (DrumGlyph ()))
drum_score = fmap simpleNoteList $
    Repeat ("a",drum_notes) $ Nil


drum :: DrumPitch -> Duration -> DrumGlyph ()
drum p drn = GlyNote (Note () p) drn False

dchord :: [DrumPitch] -> Duration -> DrumGlyph ()
dchord ps drn = Chord (fromList $ map (Note ()) ps) drn False


drum_notes :: [DrumGlyph ()]
drum_notes = [ dchord [handclap, ridecymbal] dEighth
             , drum pedalhihat dEighth
             , dchord [handclap, ridecymbal] dEighth
             , dchord [bassdrum, ridecymbal] dEighth
             -- 
             , dchord [handclap, ridecymbal] dEighth
             , drum pedalhihat dEighth
             , dchord [bassdrum, handclap, ridecymbal] dEighth
             , drum ridecymbal dEighth
             ]


