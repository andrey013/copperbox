



--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.PerformPitch
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Perform the pitch representation
-- |
--------------------------------------------------------------------------------



-- for a list of pitches turn them into events with an onset time


module Bala.Base.PerformPitch where

import Bala.Base.PitchRep
import Bala.Base.Perform

import Data.Ratio

data PitchEnv = PitchEnv {
  default_note_length :: Ratio Int
  }
  
default_env = PitchEnv {
  default_note_length = 1 / 4
  }
    