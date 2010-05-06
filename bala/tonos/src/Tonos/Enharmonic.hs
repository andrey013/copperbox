{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Tonos.Enharmonic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Neume
--
--------------------------------------------------------------------------------

module Tonos.Enharmonic
  

   where

import Tonos.Base
import Tonos.Pitch
import Tonos.Z12

import qualified Neume.Core.Pitch as Neume


type SpellingMap = Z12 -> (PitchLetter,Accidental)

-- @standardForm@ might have 3 or more accidentals, which 
-- can\'t be representated in Neume.
-- 
-- So we need some enharmonic spelling...
--

neumePitch :: SpellingMap -> Pitch -> Neume.Pitch
neumePitch spellF p = Neume.Pitch (neumePitchLetter l) (neumeAccidental a) o
  where
    (l,a,o)    = enharmonic spellF $ standardForm p

enharmonic :: SpellingMap -> (PitchLetter, Accidental, Octave) 
           -> (PitchLetter, Accidental, Octave)
enharmonic spellF (l,a,o) | abs a < 2 = (l,a,o) 
                            | otherwise = (l', a', odif+o)
  where
    (odif,n) = (a + semitones l) `divMod` 12
    (l',a')  = spellF $ fromIntegral n




neumePitchLetter :: PitchLetter -> Neume.PitchLetter
neumePitchLetter C = Neume.C
neumePitchLetter D = Neume.D
neumePitchLetter E = Neume.E
neumePitchLetter F = Neume.F
neumePitchLetter G = Neume.G
neumePitchLetter A = Neume.A
neumePitchLetter B = Neume.B

neumeAccidental :: Accidental -> Maybe Neume.Accidental
neumeAccidental   0  = Nothing
neumeAccidental   1  = Just Neume.Sharp
neumeAccidental (-1) = Just Neume.Flat
neumeAccidental   _  = error $ "neumeAccidental - accidental not normalized"


-- | g_major accounts for [0,2,4,6(F#),7,9,11]
--  
g_major :: SpellingMap
g_major 0  = (C,0)         -- C  - in scale
g_major 1  = (D,(-1))      -- A4 (G to C#) or d5 (G to Db) - no winner, use Db
g_major 2  = (D,0)         -- D  - in scale
g_major 3  = (E,(-1))      -- Eb - m6 from tonic G
g_major 4  = (E,0)         -- E  - in scale
g_major 5  = (F,0)         -- F  - favour naturals
g_major 6  = (F,1)         -- F# - in scale
g_major 7  = (G,0)         -- G  - in scale (tonic)
g_major 8  = (A,(-1))      -- Ab - m2 from tonic G
g_major 9  = (A,0)         -- A  - in scale
g_major 10 = (B,(-1))      -- Bb - m3 from tonic G
g_major _  = (B,0)         -- B  - inscale


