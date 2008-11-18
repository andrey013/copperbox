--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.MetricalEvent
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- MetricalEvent (to be replaced by Structural at some point...)
--
--------------------------------------------------------------------------------

module Bala.Base.MetricalEvent where

import Bala.Base.BaseExtra
import Bala.Base.Duration 
import Bala.Base.Pitch

data MetricalEvent a = N a Duration | R Duration
  deriving (Eq,Show)

type Beat = MetricalEvent ()
type PitchEvent = MetricalEvent Pitch

beats :: Duration -> [Beat] 
beats d = repeat (N () d)
  



              