{-# OPTIONS -Wall #-}

module Examples where

import qualified Mullein.AbcOutput as ABC
import Mullein.Bracket
import Mullein.Core
import Mullein.Duration
import Mullein.Extended
import Mullein.LilyPondOutput
import Mullein.NamedElements
import Mullein.Pitch

import Text.PrettyPrint.Leijen

import Data.Ratio

--------------------------------------------------------------------------------
-- Example 1 
-- The first four bars from Bulgarian 6 in the ABC exotic song book.
-- Glyphs are just the standard glyph - Glyph Pitch Duration
-- Standard glyphs can be print by ABC and LilyPond.


demo1 :: Doc
demo1 = simpleOutput $ oPhrase $ rewritePitch middle_c $ rewriteDuration xs
  where
    xs = phrase twoFourTime b6_bars1'4

demo1a :: Doc
demo1a = ABC.simpleOutput $ ABC.oPhrase 
                          $ ABC.rewritePitch amaj
                          $ ABC.rewriteDuration (1%16) xs 
  where
   xs   = phrase twoFourTime b6_bars1'4
   amaj = makeSpellingMap 3



twoFourTime :: MeterPattern
twoFourTime = meterPattern 2 4



b6_bars1'4 :: [PDGlyph]
b6_bars1'4 =  
  [ a 4 sn, b 4 sn, cs 5 sn, cs 5 sn, cs 5 sn, a 4 sn, 
               cs 5 sn, cs 5 sn
  -- bar 2
  , cs 5 sn, a 4 sn, b 4 sn, cs 5 sn, b 4 sn, a 4 sn, 
                a 4 sn, snr
  -- bar 3
  , e 5 sn, d 5 sn, cs 5 sn, b 4 sn, cs 5 sn, a 4 sn, 
               b 4 sn, cs 5 sn
  -- bar 4
  , a 4 sn, b 4 sn, b 4 sn, a 4 sn, a 4 en, enr
  ]

--------------------------------------------------------------------------------
-- Example 2
-- LilyPond Percussion

demo2 :: Doc
demo2 = simpleOutput $ oPhrase $ rewriteDuration xs
  where
    xs = phrase fourFourTime $ drums1


fourFourTime :: MeterPattern
fourFourTime = meterPattern 4 4

drum :: DrumPitch -> Duration -> DrumGlyph
drum p drn = Note p drn

drums1 :: [DrumGlyph]
drums1 = [drum snare qn, qnr, drum snare qn, qnr]


--------------------------------------------------------------------------------
-- Example 3
-- Fingering annotations


demo3 :: Doc
demo3 = simpleOutput $ oPhrase $ rewritePitch middle_c $ rewriteDuration xs
  where
    xs :: Phrase FingeredGlyph
    xs = phrase fourFourTime $ two_chords


two_chords :: [FingeredGlyph]
two_chords = [ f 5 sn %% 2, a 5 sn %% 1, d 6 sn %% 3, g 6 sn %% 4]

