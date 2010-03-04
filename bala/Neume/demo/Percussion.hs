

module Percussion where

import Neume.Bracket
import Neume.Datatypes
import Neume.Doc
import Neume.Duration
import Neume.Extended
import Neume.LilyPondDoc
import Neume.LilyPondOutput
import Neume.NamedElements
import Neume.Pitch
import Neume.SyntaxStaff
import Neume.Utils

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd


demo1 = printDoc ly_score

main :: IO ()
main = do 
  writeDoc "percussion.ly"      ly_score
  system   "lilypond percussion.ly"
  return ()


{- 
demo2 :: Doc
demo2 = simpleOutput $ renderPhrase $ rewriteDuration xs
  where
    xs = phrase (meterPattern four_four_time) drums1
-}


ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> new "DrumStaff" (drummode tune)
  where
    tune =  simpleOutput $ renderPhrase (text . drumShortName)      
                         $ rewriteDurationOpt xs

    xs   :: StaffPhrase DrumGlyph
    xs   = phrase (meterPattern four_four_time) $ simpleNoteList drum_tune

drum :: DrumPitch -> Duration -> DrumGlyph
drum p drn = GlyNote (Note () p drn) False

drum_tune :: [DrumGlyph]
drum_tune = [drum snare qn, drum hihat qn, drum snare qn, drum hihat qn]


