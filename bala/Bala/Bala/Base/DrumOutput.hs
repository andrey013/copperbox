{-# OPTIONS -Wall #-}

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

import qualified HNotate as H
import qualified ZMidi as Z (GMDrum(..), drumPitch)

class DrumMapping a where
  gmDrum    :: a        -> Z.GMDrum
  drumName  :: H.Pitch  -> Maybe a
  drumPitch :: a        -> H.Pitch 
  
  drumPitch = gmDrumPitch . gmDrum
  
gmDrumPitch :: Z.GMDrum -> H.Pitch
gmDrumPitch = H.fromSemitones . fromIntegral . Z.drumPitch

pitchToGmDrum :: H.Pitch -> Maybe Z.GMDrum
pitchToGmDrum = fn . H.semitones where
  fn i | i >= 35 && i <= 81   = Just $ toEnum $ i -35
       | otherwise            = Nothing 
       
        