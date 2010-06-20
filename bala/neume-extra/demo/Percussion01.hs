{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

module Percussion01 where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondOutput
import Neume.Core.Syntax
import Neume.Core.Utils.OneList ( fromList )
import Neume.Core.Utils.Pretty ( writeDoc )
import Neume.Extra.Common
import Neume.Extra.DrumPitches
import Neume.Extra.LilyPondDoc
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
drum_parts_doc = outputParts $ scoreZipWith fn drum_score_final name_score
  where
    fn img name = (name,img)

score_doc :: Doc 
score_doc = outputScore name_score


{-
-- The original Neume had better code for this...
--
outputParts :: Score shape (String,PhraseImage) -> Doc
outputParts = undefined -- scoreFoldr empty lin rep repalt
  where
    lin    (s,img)      ac = def id s img  <$> ac 
    rep    (s,img)      ac = def (repeatvolta 2) s img <$> ac
    repalt _       _    _  = error "outputParts - repalt"
    def f s img            = variableDef s (drummode $ f $ vsep $ getPhraseData img)
-}

outputScore :: Score shape String -> Doc
outputScore scr = scoreExpr $ new "DrumStaff" $ nestBraces body
  where
    body = fillSep $ map variableUse $ "drumglobals" : content scr

-- the score here doesn't need naming...

-- This one doesn't merit using Score datatype...

makeScore :: a -> Score (TRepeat :. Z) a
makeScore a = Repeat a $ Nil

drum_score_final :: Score (TRepeat :. Z) PhraseImage
drum_score_final = fmap trafo drum_score
  where 
    trafo = runRender (renderGlyph drumShortName strip)
              . drumScoreTrafo . makeFull (bracketConfig [1%2,1%2])

name_score :: Score (TRepeat :. Z) String
name_score = makeScore "chacha"

drum_score :: Score (TRepeat :. Z) (SimpleNoteList (DrumGlyph () Duration))
drum_score = makeScore (simpleNoteList $ chacha_notes)

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


