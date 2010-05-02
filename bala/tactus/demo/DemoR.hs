{-# LANGUAGE TypeOperators              #-}

-- > :set -i../src:../../Neume/src

module DemoR where

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

hijaz_mp :: MeterPattern 
hijaz_mp = [(2,8),(2,8),(3,8)]


drum_notes :: [DrumGlyph ()]
drum_notes = runAlg fn hijaz_mp one_d_d
  where
    fn = hc . maybe (error "bad conv") id . rationalToDuration

hc :: Duration -> DrumGlyph ()
hc drn = GlyNote (Note () handclap) drn False

-----

one_d_d :: Alg a
one_d_d = one +++ dim +++ dim

demo1 = runAlg id hijaz_mp one_d_d

demo2 = runAlg id hijaz_mp (aug 3)

demo3 :: [Double]
demo3 = runAlg realToFrac hijaz_mp (aug 3)

demo4 = runAlg id hijaz_mp (one +++ one +++ div2 (\_ -> (1,2)))
