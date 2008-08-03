
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Base.Class
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

module Bala.Perform.Base.Class( 
  Perform(..),
  PitchRepr(..),
  DurationRepr(..),
  ) where

import Bala.Perform.Base.Datatypes

class Perform evt where eventvalues :: evt -> (Maybe Pitch, Maybe Duration)

class PitchRepr pch where renderPitch :: pch -> Pitch
class DurationRepr dur where renderDuration :: dur -> Duration


  
  