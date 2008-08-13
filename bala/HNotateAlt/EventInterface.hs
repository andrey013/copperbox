
--------------------------------------------------------------------------------
-- |
-- Module      :  EventInterface
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

module EventInterface (
    Event(..),
    PitchRepr(..),
    DurationRepr(..),
  ) where

import Duration
import Pitch


class Event evt where eventvalues :: evt -> (Maybe Pitch, Maybe Duration)

class PitchRepr pch where renderPitch :: pch -> Pitch
class DurationRepr dur where renderDuration :: dur -> Duration

-- Make Pitch representable as itself  
instance PitchRepr Pitch where 
  renderPitch = id  
  
-- Make Duration representable as itself  
instance DurationRepr Duration where 
  renderDuration = id  

