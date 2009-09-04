{-# OPTIONS -Wall #-}

module Examples where

import qualified Mullein.AbcDoc    as ABC
import qualified Mullein.AbcOutput as ABC
import Mullein.Bracket
import Mullein.Core
import Mullein.Duration
import Mullein.Extended
import Mullein.LilyPondDoc
import Mullein.LilyPondOutput
import Mullein.NamedElements
import Mullein.Pitch
import Mullein.Utils ( writeDoc )

import Text.PrettyPrint.Leijen

import Data.Ratio

--------------------------------------------------------------------------------
-- Example 1 
-- The first four bars from Bulgarian 6 in the ABC exotic song book.
-- Glyphs are just the standard glyph - Glyph Pitch Duration
-- Standard glyphs can be print by ABC and LilyPond.


demo1 :: Doc
demo1 =  version "2.12.2" 
     <$> score (relative middle_c $ key a_nat "major" <$> time 2 4 <$> tune)
  where
    tune = simpleOutput $ renderPhrase $ rewritePitch middle_c $ rewriteDuration xs
    xs   = phrase twoFourTime b6_bars1'4

demo1a :: Doc
demo1a =  ABC.tunenum   1 
      <$> ABC.title     "Bulgarian 6"
      <$> ABC.meter     "2/4"
      <$> ABC.key       "Amaj"
      <$> tune
  where
    tune = ABC.simpleOutput $ ABC.renderPhrase 
                            $ ABC.rewritePitch amaj
                            $ ABC.rewriteDuration (1%16) xs 
    xs   = phrase twoFourTime b6_bars1'4
    amaj = makeSpellingMap 3

output1 :: IO ()
output1 = do 
  writeDoc "bulgarian6.ly"  demo1
  writeDoc "bulgarian6.abc" demo1a


twoFourTime :: MeterPattern
twoFourTime = makeMeterPattern 2 4



b6_bars1'4 :: [PDGlyph]
b6_bars1'4 =  
  [ a 5 sn, b 5 sn, cs 6 sn, cs 6 sn, cs 6 sn, a 5 sn, 
            cs 6 sn, cs 6 sn
  -- bar 2
  , cs 6 sn, a 5 sn, b 5 sn, cs 6 sn, b 5 sn, a 5 sn, 
             a 5 sn, snr
  -- bar 3
  , e 6 sn, d 6 sn, cs 6 sn, b 5 sn, cs 6 sn, a 5 sn, 
            b 5 sn, cs 6 sn
  -- bar 4
  , a 5 sn, b 5 sn, b 5 sn, a 5 sn, a 5 en, enr
  ]

--------------------------------------------------------------------------------
-- Example 2
-- LilyPond Percussion

demo2 :: Doc
demo2 = simpleOutput $ renderPhrase $ rewriteDuration xs
  where
    xs = phrase fourFourTime $ drums1


fourFourTime :: MeterPattern
fourFourTime = makeMeterPattern 4 4

drum :: DrumPitch -> Duration -> DrumGlyph
drum p drn = Note p drn False

drums1 :: [DrumGlyph]
drums1 = [drum snare qn, qnr, drum snare qn, qnr]


--------------------------------------------------------------------------------
-- Example 3
-- Fingering annotations


demo3 :: Doc
demo3 = simpleOutput $ renderPhrase $ rewritePitch middle_c $ rewriteDuration xs
  where
    xs :: Phrase FingeredGlyph
    xs = phrase fourFourTime $ two_chords


two_chords :: [FingeredGlyph]
two_chords = [ f 5 sn %% 2, a 5 sn %% 1, d 6 sn %% 3, g 6 sn %% 4]

