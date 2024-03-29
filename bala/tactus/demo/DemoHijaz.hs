{-# LANGUAGE TypeOperators              #-}

-- > :set -i../src:../../Neume/src

module DemoHijaz where

import Tactus.Base
import Tactus.Fraction
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


drum_score :: Score (TRepeat :. TRepAlt :. TRepAlt :. TLinear 
                             :. TRepeat :. TRepAlt :. TRepAlt 
                             :. TRepAlt :. Z) 
                    (NoteList (DrumGlyph ()))
drum_score = fmap simpleNoteList $
      Repeat ("a",bars_1_4 ) 
    $ RepAlt ("b",bars_5_7)    [("bFirst", bar_8), ("bSecond", bar_9)] 
    $ RepAlt ("c",bars_10_12)  [("cFirst", bar_13), ("cSecond", bar_14)] 
    $ Linear ("d",bars_15_16)
    $ Repeat ("e",bars_17_20)
    $ RepAlt ("f",bars_21_23)  [("fFirst", bar_24), ("fSecond", bar_25)] 
    $ RepAlt ("g",bars_26_28)  [("gFirst", bar_29), ("gSecond", bar_30)] 
    $ RepAlt ("h",bars_31_33)  [("hFirst", bar_34), ("hSecond", bar_35)] 
    $ Nil
  


hijaz_mp :: MeterPattern 
hijaz_mp = [(2:%:8),(2:%:8),(3:%:8)]


bars_1_4    :: [DrumGlyph ()]
bars_1_4    = drumNotes $ o_d_d +++ o_o_d +++ o_d_d +++ o_o_o

bars_5_7    :: [DrumGlyph ()]
bars_5_7    = drumNotes $ o_d_d +++ o_o_d +++ o_d_d

bar_8       :: [DrumGlyph ()]
bar_8       = drumNotes o_d_o
 
bar_9       :: [DrumGlyph ()]
bar_9       = drumNotes o_o_o


bars_10_12  :: [DrumGlyph ()]
bars_10_12  = drumNotes $ o_d_d2'1 +++ o_d_d2'1 +++ o_d_d


bar_13      :: [DrumGlyph ()]
bar_13      = drumNotes o_d_d
 
bar_14      :: [DrumGlyph ()]
bar_14      = drumNotes o_o_o

bars_15_16  :: [DrumGlyph ()]
bars_15_16  = drumNotes $ o_o_d +++ o_o_d
 
bars_17_20  :: [DrumGlyph ()]
bars_17_20  = drumNotes $ o_d_d +++ o_d_d2'1 +++ o_d_d +++ d_d_o

bars_21_23  :: [DrumGlyph ()]
bars_21_23  = drumNotes $ d_special +++ d_special +++ d_d_d2'1

bar_24      :: [DrumGlyph ()]
bar_24      = drumNotes d_d_d
 
bar_25      :: [DrumGlyph ()]
bar_25      = drumNotes o_o_o

bars_26_28  :: [DrumGlyph ()]
bars_26_28  = drumNotes $ d_d_d2'1 +++ d_d_d +++ o_d_d

bar_29      :: [DrumGlyph ()]
bar_29      = drumNotes o_d_o

bar_30      :: [DrumGlyph ()]
bar_30      = drumNotes o_o_o

bars_31_33  :: [DrumGlyph ()]
bars_31_33  = drumNotes $ o_o_d +++ o_o_d +++ o_d_d

bar_34      :: [DrumGlyph ()]
bar_34      = drumNotes o_d_o

bar_35      :: [DrumGlyph ()]
bar_35      = drumNotes o_o_o


drumNotes :: Alg (DrumGlyph ()) -> [DrumGlyph ()]
drumNotes = runAlg fn hijaz_mp 
  where
    fn = hc . maybe (error "bad conv") id . fractionToDuration

hc :: Duration -> DrumGlyph ()
hc drn = GlyNote (Note () handclap) drn False

d_d_d       :: Alg a
d_d_d       = dim +++ dim +++ dim

d_d_o       :: Alg a
d_d_o       = dim +++ dim +++ one

o_d_d       :: Alg a
o_d_d       = one +++ dim +++ dim

o_o_d       :: Alg a
o_o_d       = one +++ dim +++ dim

o_o_o       :: Alg a
o_o_o       = one +++ one +++ one

o_d_o       :: Alg a
o_d_o       = one +++ dim +++ one

d_d_d2'1    :: Alg a
d_d_d2'1    = dim +++ dim +++ divide2 (2,1)

o_d_d2'1    :: Alg a
o_d_d2'1    = one +++ dim +++ divide2 (2,1)

d_special :: Alg a
d_special = dim +++ divide3 (2,1,1) +++ divide3 (4,1,1)