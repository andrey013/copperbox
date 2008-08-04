
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Base.Class
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Type classes for `performing` a music representation
-- The Score renderer and backends are parametrized on pitch and duration
-- So the can output for other music representations not just Bala.
--
--------------------------------------------------------------------------------

module HNotate.Base.Class(
    Event(..),
    PitchRepr(..),
    DurationRepr(..),
  ) where

import HNotate.Base.Datatypes

class Event evt where eventvalues :: evt -> (Maybe Pitch, Maybe Duration)

class PitchRepr pch where renderPitch :: pch -> Pitch
class DurationRepr dur where renderDuration :: dur -> Duration



