{-# LANGUAGE TypeOperators              #-}

-- > :set -i../src:../../Neume/src

module DemoHijaz where

import Tactus.Base
import Tactus.Neume

import Neume.Core.Duration
import Neume.Core.LilyPondFormat
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
import Neume.Core.Utils 
import Neume.Extra.DrumPitches
import Neume.Extra.LilyPondDoc
import Neume.Extra.Percussion

import Data.Ratio
import System.Cmd

import Text.PrettyPrint.Leijen

main :: IO ()
main =
  writeDoc "hijaz-perc.ly"      ly_score        >>
  system   "lilypond hijaz-perc.ly"             >>
  return ()



ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> drumtune
        <$> scoreExpr (new "DrumStaff" 
                        (with dstyle (simultaneous [variableUse "drumtune"])))
  where
    dstyle =  definition "drumStyleTable" (text "#percussion-style")
          <$> override "StaffSymbol" "line-count" (int 1)
          <$> override "BarLine"     "bar-size"   (int 3)

drumtune :: Doc
drumtune = variableDef "drumtune" $ drummode (time 7 8 <$> stemDown <$> tune1 )
  where
    tune1    = renderLyDrums ofmt orw drum_score
        
    ofmt     = Ly_std_format_config       strip
    orw      = Ly_drums_rewrite_config [2%8, 2%8, 3%8] strip


drum_score :: Score (TRepeat :. TRepAlt :. Z) (NoteList (DrumGlyph ()))
drum_score = fmap simpleNoteList $
      Repeat ("a",bars_1_4 ) 
    $ RepAlt ("b",bars_5_7) [("bFirst", bar_8),("bSecond", bar_9)] 
    $ Nil
  


hijaz_mp :: MeterPattern 
hijaz_mp = [(2,8),(2,8),(3,8)]


bars_1_4 :: [DrumGlyph ()]
bars_1_4 = drum_notes (o_d_d +++ o_o_d +++ o_d_d +++ o_o_o)

bars_5_7 :: [DrumGlyph ()]
bars_5_7 = drum_notes (o_d_d +++ o_o_d +++ o_d_d)

bar_8    :: [DrumGlyph ()]
bar_8 = drum_notes o_d_o
 
bar_9    :: [DrumGlyph ()]
bar_9 = drum_notes o_o_o


drum_notes :: Alg (DrumGlyph ()) -> [DrumGlyph ()]
drum_notes = runAlg fn hijaz_mp 
  where
    fn = hc . maybe (error "bad conv") id . fractionToDuration

hc :: Duration -> DrumGlyph ()
hc drn = GlyNote (Note () handclap) drn False



o_d_d :: Alg a
o_d_d = one +++ dim +++ dim

o_o_d :: Alg a
o_o_d = one +++ dim +++ dim

o_o_o :: Alg a
o_o_o = one +++ one +++ one

o_d_o :: Alg a
o_d_o = one +++ dim +++ one
