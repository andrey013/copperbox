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

module Bala.Scale where

import Bala.Interval
import Bala.Pitch

import Data.AffineSpace


data Scale = Scale { scaleRoot :: Pitch, scaleIntervals :: [Interval] }
  deriving (Eq)


--------------------------------------------------------------------------------


majorScale :: Pitch -> Scale
majorScale = Scale `flip` xs where
  xs = [perfect1, major2, major3, perfect4, perfect5, major6, major7, perfect8]


naturalMinorScale :: Pitch -> Scale
naturalMinorScale = Scale `flip` xs where
  xs = [perfect1, major2, minor3, perfect4, perfect5, minor6, minor7, perfect8]


extractPitches :: Scale -> [Pitch]
extractPitches (Scale r ivs) = map (r .+^) ivs