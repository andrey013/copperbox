
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.DrumOutput
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Duration representation - re-exports HNotate.Duration
--
--------------------------------------------------------------------------------

module Bala.Base.DrumOutput where

import Bala.Base.Pitch

import qualified HNotate as H
import qualified HNotate.Marks as H
import qualified HNotate.NoteListDatatypes as H
import qualified ZMidi as Z (GMDrum(..), drumPitch)

class DrumMapping a where
  gmDrum    :: a      -> Z.GMDrum
  drumName  :: Pitch  -> Maybe a
  drumPitch :: a      -> Pitch 
  
  drumPitch = gmDrumPitch . gmDrum
  
gmDrumPitch :: Z.GMDrum -> Pitch
gmDrumPitch = fromSemitones . fromIntegral . Z.drumPitch

pitchToGmDrum :: Pitch -> Maybe Z.GMDrum
pitchToGmDrum = fn . semitones where
  fn i | i >= 35 && i <= 81   = Just $ toEnum $ i -35
       | otherwise            = Nothing 
       
        