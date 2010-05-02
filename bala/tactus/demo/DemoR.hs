{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wall #-}

-- > :set -i../src:../../Neume/src

module DemoR where

import Tactus.Base
import Tactus.Fraction
import Tactus.Neume

import Neume.Core.Duration
import Neume.Core.SyntaxGlyph
import Neume.Extra.DrumPitches
import Neume.Extra.Percussion

-- import Data.Ratio

hijaz_mp :: MeterPattern 
hijaz_mp = [(2:%:8),(2:%:8),(3:%:8)]


drum_notes :: [DrumGlyph ()]
drum_notes = runAlg fn hijaz_mp one_d_d
  where
    fn = hc . maybe (error "bad conv") id . fractionToDuration

hc :: Duration -> DrumGlyph ()
hc drn = GlyNote (Note () handclap) drn False

-----

one_d_d :: Alg a
one_d_d = one +++ dim +++ dim


demo1 = runAlg id hijaz_mp one_d_d

demo2 = runAlg id hijaz_mp (aug 3)

demo3 = runAlg id hijaz_mp (one +++ one +++ divide2 (1,2))
