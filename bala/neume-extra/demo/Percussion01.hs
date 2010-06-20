{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

module Percussion01 where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.Syntax
import Neume.Core.Utils.OneList ( fromList )
import Neume.Core.Utils.Pretty ( writeDoc )

import Neume.Extra.DrumPitches
import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondScoreOutput
import Neume.Extra.Percussion
import Neume.Extra.ScoreSyntax

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
        <$> drum_globals_doc
        <$> drum_parts_doc
        <$> score_doc

drum_globals_doc :: Doc
drum_globals_doc = 
    variableDef "drumglobals" $ nestBraces (time 4 4 <$> stemUp)

drum_parts_doc :: Doc
drum_parts_doc = defnsDefns barNumber 1 drum_plan drum_score_final

drum_plan :: ScorePlan (TRepeat :. Z) DefinitionsElement
drum_plan = PRepeat chacha PNil
  where
    chacha    = ("chacha", chachaF)
    chachaF d = drummode $ time 4 4 <$> stemUp <$> d


score_doc :: Doc 
score_doc = scoreExpr $ new "DrumStaff" $ nestBraces $ 
              variableUse "drumglobals" <$> defnsScore drum_plan


outputScore :: Score shape String -> Doc
outputScore scr = scoreExpr $ new "DrumStaff" $ nestBraces body
  where
    body = fillSep $ map variableUse $ "drumglobals" : content scr

-- the score here doesn't need naming...

-- This one doesn't merit using Score datatype...

makeScore :: a -> Score (TRepeat :. Z) a
makeScore a = Repeat a $ Nil

drum_score_final :: Score (TRepeat :. Z) PhraseImage
drum_score_final = 
    lilyPondImageScore (drumAlg drumShortName) $ fmap trafo drum_score
  where 
    trafo = makeFull (bracketConfig [1%2,1%2])


drum_score :: Score (TRepeat :. Z) (SimpleNoteList (DrumGlyph () Duration))
drum_score = makeScore (simpleNoteList $ chacha_notes)


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


