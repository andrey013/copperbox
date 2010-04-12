{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

module Percussion where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondBasic
import Neume.Core.LilyPondOutput
import Neume.Core.Pitch
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
import Neume.Core.Utils
import Neume.Core.Utils.OneList ( fromList )

import Neume.Extra.DrumPitches
import Neume.Extra.Extended
import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondFormat
import Neume.Extra.NamedElements

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd


demo1 = printDoc ly_score

main :: IO ()
main = do 
  writeDoc "percussion.ly"      ly_score
  system   "lilypond percussion.ly"
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
    tune1    = renderLyDrums ofmt [1%2, 1%2] drum_score
    
    ofmt     = Ly_Std_Format_Config       strip


drum_score :: Score (TRepeat :. Z) (NoteList DrumGlyph)
drum_score = fmap simpleNoteList $
    Repeat ("a",drum_notes) $ Nil


drum :: DrumPitch -> Duration -> DrumGlyph
drum p drn = GlyNote (Note () p drn) False

dchord :: [DrumPitch] -> Duration -> DrumGlyph
dchord ps drn = Chord (fromList $ map (ChordPitch ()) ps) drn False


drum_notes :: [DrumGlyph]
drum_notes = [ dchord [handclap, ridecymbal] en
             , drum pedalhihat en
             , dchord [handclap, ridecymbal] en
             , dchord [bassdrum, ridecymbal] en
             -- 
             , dchord [handclap, ridecymbal] en
             , drum pedalhihat en
             , dchord [bassdrum, handclap, ridecymbal] en
             , drum ridecymbal en
             ]


