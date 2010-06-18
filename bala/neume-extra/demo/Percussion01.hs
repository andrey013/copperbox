{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

module Percussion01 where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondOutput
import Neume.Core.Metrical
import Neume.Core.Syntax
import Neume.Core.Utils.OneList ( fromList )

import Neume.Extra.Common
import Neume.Extra.DrumPitches
import Neume.Extra.LilyPondDoc
import Neume.Extra.Percussion
import Neume.Extra.ScoreSyntax

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd


{-
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
-}

-- the score here doesn't need naming...

-- This one doesn't merit using Score datatype...

drum_score_one = fmap trafo drum_score_zero
  where 
    trafo = runRender (renderGlyph drumShortName strip)
              . drumScoreTrafo . makeFull (bracketConfig [1%2,1%2])

drum_score_zero :: Score (TRepeat :. Z) 
                         (SimpleNoteList (DrumGlyph () Duration))
drum_score_zero = Repeat (simpleNoteList $ chacha_notes) $ Nil

-- fmap (Full . phrase (bracketConfig [1%2,1%2])) $ 

simpleNoteList :: [e] -> SimpleNoteList e
simpleNoteList = NoteList . map Item

drum :: DrumPitch -> Duration -> DrumGlyph () Duration
drum p drn = GlyNote (Note () p) drn NoTie

dchord :: [DrumPitch] -> Duration -> DrumGlyph () Duration
dchord ps drn = Chord (fromList $ map (Note ()) ps) drn NoTie


chacha_notes :: [DrumGlyph () Duration]
chacha_notes = 
    [ dchord [handclap, ridecymbal] dEighth
    , drum pedalhihat dEighth
    , dchord [handclap, ridecymbal] dEighth
    , dchord [bassdrum, ridecymbal] dEighth
    -- 
    , dchord [handclap, ridecymbal] dEighth
    , drum pedalhihat dEighth
    , dchord [bassdrum, handclap, ridecymbal] dEighth
    , drum ridecymbal dEighth
    ]


