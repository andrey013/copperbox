{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Extended
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Extended music representation datatypes (e.g. Pitch with fingering)
--
--------------------------------------------------------------------------------

module Mullein.Extended where

import Mullein.Core
import Mullein.Duration
import Mullein.LilyPondOutput
import Mullein.Pitch


import Text.PrettyPrint.Leijen

--------------------------------------------------------------------------------
-- Notes with fingering annotations

type Finger = Int

-- Fingering is optional - numering every note would clutter up
-- scores, especially where notes are repeated.

data FingeredPitch = FingeredPitch { 
       finPitch    ::Pitch, 
       finNumber   :: Maybe Finger
     }
  deriving (Eq,Show)

type FingeredGlyph = Glyph FingeredPitch Duration


(%%) :: FingeredGlyph -> Int -> FingeredGlyph
(Note (FingeredPitch p _) drn) %% i = Note (FingeredPitch p (Just i)) drn
a                              %% _ = a



instance HasPitch FingeredPitch where
  getPitch (FingeredPitch p _)= p
  setPitch p (FingeredPitch _ oi) = FingeredPitch p oi


instance MakeNote FingeredGlyph where
  makeNote pch drn = Note (FingeredPitch pch Nothing) drn

instance MakeRest FingeredGlyph where
  makeRest drn = Rest drn


instance LilyPondGlyph (Glyph FingeredPitch (Maybe Duration)) where
  lyGlyph (Note p d)       = pitchDurationFinger p d
  lyGlyph (Rest d)         = rest d
  lyGlyph (Spacer d)       = spacer d
  lyGlyph (Chord ps d)     = chordForm (map pitchFinger ps) d
  lyGlyph (GraceNotes xs)  = graceForm $ map fn xs 
    where fn (GraceNote p d) = pitchDurationFinger p d
  lyGlyph Tie              = tie


pitchDurationFinger :: FingeredPitch -> Maybe Duration -> Doc
pitchDurationFinger (FingeredPitch p mi) md = 
  pitch p <> optDuration md <> optFingering mi

pitchFinger :: FingeredPitch -> Doc
pitchFinger (FingeredPitch p mi) = pitch p <> optFingering mi

optFingering :: Maybe Finger -> Doc
optFingering = maybe empty (\i -> char '-' <> int i) 











