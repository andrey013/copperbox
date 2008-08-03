
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Midi.Class
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Type classes that must be implemented to emit Midi from Score.
--
--------------------------------------------------------------------------------

module Bala.Perform.Midi.Class (PitchMidi(..), DurationMidi(..)) where

import Data.Word

class PitchMidi pch where
  midiPitch   :: pch -> Word8
  
class DurationMidi dur where
  midiTicks             :: dur -> Integer

  
  