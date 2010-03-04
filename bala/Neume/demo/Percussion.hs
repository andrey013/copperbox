

module Percussion where

import Neume.Bracket
import Neume.Datatypes
import Neume.Doc
import Neume.Duration
import Neume.Extended
import Neume.LilyPondDoc
import Neume.LilyPondOutput
import Neume.NamedElements
import Neume.OneList ( fromList )
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
        <$> drumtune
        <$> scoreExpr (new "DrumStaff" 
                        (with dstyle (simultaneous [variableUse "drumtune"])))
  where
    dstyle = definition "drumStyleTable" (text "#drums-style")


drumtune :: Doc
drumtune = variableDef "drumtune" $ drummode (time 4 4 <$> stemUp <$> tune )
  where
    tune =  simpleOutput $ renderPhrase (text . drumShortName)      
                         $ rewriteDurationOpt xs

    xs   :: StaffPhrase DrumGlyph
    xs   = phrase [1%2, 1%2] $ simpleNoteList drum_tune

drum :: DrumPitch -> Duration -> DrumGlyph
drum p drn = GlyNote (Note () p drn) False

dchord :: [DrumPitch] -> Duration -> DrumGlyph
dchord ps drn = Chord (fromList $ map (ChordPitch ()) ps) drn False


drum_tune :: [DrumGlyph]
drum_tune = [ dchord [handclap, ridecymbal] en
            , drum pedalhihat en
            , dchord [handclap, ridecymbal] en
            , dchord [bassdrum, ridecymbal] en
            -- 
            , dchord [handclap, ridecymbal] en
            , drum pedalhihat en
            , dchord [bassdrum, handclap, ridecymbal] en
            , drum ridecymbal en
            ]


