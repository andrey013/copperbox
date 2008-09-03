
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.EventInterface
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Type classes for `performing` a music representation
-- The Score renderer and backends are parametrized on pitch and duration
-- so they can produce output different music representations not just Bala.
--
--------------------------------------------------------------------------------

module HNotate.EventInterface (
    Event(..),
    PitchRepr(..),
    DurationRepr(..),
  ) where

import HNotate.Duration
import HNotate.Pitch


class Event evt where eventvalues :: evt -> (Maybe Pitch, Maybe Duration)

class PitchRepr pch where renderPitch :: pch -> Pitch
class DurationRepr dur where renderDuration :: dur -> Duration

-- Make HNotate's internal pitch representable as itself.  
instance PitchRepr Pitch where 
  renderPitch = id  
  
-- Make HNotate's internal duration representable as itself. 
instance DurationRepr Duration where 
  renderDuration = id  

