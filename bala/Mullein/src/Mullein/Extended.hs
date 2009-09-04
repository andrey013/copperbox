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
-- Extended music representation datatypes (e.g. pitch with fingering)
--
--------------------------------------------------------------------------------

module Mullein.Extended 
  (
  -- * Notes with fingering annotations
    Finger
  , FingeredPitch(..)
  , FingeredGlyph
  , (%%)

  ) where

import Mullein.Core
import Mullein.Duration
import Mullein.LilyPondDoc
import Mullein.LilyPondOutput
import Mullein.Pitch
import Mullein.Utils ( optDoc )

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
(Note (FingeredPitch p _) drn t) %% i = Note (FingeredPitch p (Just i)) drn t
a                                %% _ = a



instance HasPitch FingeredPitch where
  getPitch (FingeredPitch p _)= p
  setPitch p (FingeredPitch _ oi) = FingeredPitch p oi


instance MakeNote FingeredGlyph where
  makeNote pch drn = Note (FingeredPitch pch Nothing) drn False

instance MakeRest FingeredGlyph where
  makeRest drn = Rest drn


instance LilyPondGlyph (Glyph FingeredPitch (Maybe Duration)) where
  lyGlyph (Note p d t)     = pitchDurationFinger p d <> optDoc t tie
  lyGlyph (Rest d)         = rest d
  lyGlyph (Spacer d)       = spacer d
  lyGlyph (Chord ps d t)   = chordForm (map pitchFinger ps) d <> optDoc t tie
  lyGlyph (GraceNotes xs)  = graceForm $ map fn xs 
    where fn (GraceNote p d) = pitchDurationFinger p d


pitchDurationFinger :: FingeredPitch -> Maybe Duration -> Doc
pitchDurationFinger (FingeredPitch p mi) md = 
    pitch p <> maybe empty duration md <> maybe empty fingering mi

pitchFinger :: FingeredPitch -> Doc
pitchFinger (FingeredPitch p mi) = pitch p <> maybe empty fingering mi

fingering :: Finger -> Doc
fingering i = char '-' <> int i











