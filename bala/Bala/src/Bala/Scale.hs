{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Scale
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Scale representation 
--
--------------------------------------------------------------------------------

module Bala.Scale 
  ( 
  -- * Datatypes
    Scale

  -- * Operations
  , extractPitches

  -- * Named scales
  , majorScale
  , naturalMinorScale
  , ionianMode
  , dorianMode
  , phrygianMode
  , lydianMode
  , mixolydianMode
  , aeolianMode
  , locrianMode
  , majorPentatonicBlues
  , minorPentatonicBlues

  ) where

import Bala.Interval
import Bala.Pitch

import Data.AffineSpace

import Data.Monoid

data Scale = Scale { scaleRoot :: Pitch, scaleIntervals :: [Interval] }
  deriving (Eq)


--------------------------------------------------------------------------------




extractPitches :: Scale -> [Pitch]
extractPitches (Scale r ivs) = map (r .+^) ivs

literalScale :: [Interval] -> Pitch -> Scale
literalScale = flip Scale

stepScale :: [Interval] -> Pitch -> Scale
stepScale xs = Scale `flip`  ys where
  ys = scanl mappend perfect1 xs



majorScale :: Pitch -> Scale
majorScale = literalScale $ 
  [perfect1, major2, major3, perfect4, perfect5, major6, major7, perfect8]


naturalMinorScale :: Pitch -> Scale
naturalMinorScale = literalScale $
  [perfect1, major2, minor3, perfect4, perfect5, minor6, minor7, perfect8]

ionianMode :: Pitch -> Scale
ionianMode = majorScale

dorianMode :: Pitch -> Scale
dorianMode = stepScale $ 
  [major2, minor2, major2, major2, major2, minor2, major2]

phrygianMode :: Pitch -> Scale
phrygianMode = stepScale $ 
  [minor2, major2, major2, major2, minor2, major2, major2]

lydianMode :: Pitch -> Scale
lydianMode = stepScale $ 
  [major2, major2, major2, minor2, major2, major2, minor2]

mixolydianMode :: Pitch -> Scale
mixolydianMode = stepScale $ 
  [major2, major2, minor2, major2, major2, minor2, major2]

aeolianMode :: Pitch -> Scale
aeolianMode = naturalMinorScale

locrianMode :: Pitch -> Scale
locrianMode = stepScale $ 
  [minor2, major2, major2, minor2, major2, major2, major2]


majorPentatonicBlues :: Pitch -> Scale
majorPentatonicBlues = stepScale $
  [major2, major2, minor3, major2, minor3]

minorPentatonicBlues :: Pitch -> Scale
minorPentatonicBlues = stepScale $
  [minor3, major2, major2, minor3, major2]
