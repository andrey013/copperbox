--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.BaseInstances
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Use datatypes from Bala.Base libraries to implement the render interfaces.
--
--------------------------------------------------------------------------------


module Bala.Perform.BaseInstances where

import Bala.Base
import Bala.Format.Output.OutputLilyPond
import Bala.Perform.PerformBase

instance ScDuration Duration where
  toDouble = durationSize 
  fromDouble = rationalDuration . toRational  
  
  
instance LilyPondPitch Pitch where
  middleC             = c4
  
  octaveDist p p' = 
      let ivl   = intervalType $ pitchDifference p p'
          dist  = ceiling $ (fromIntegral (ivl - 4) / 8.0)
      in if p > p' then (negate dist) else dist
  
  mkPitchName         = toEnum . fromEnum . pitchLetter
  
  mkAccidental        = fn . pitchAccidental 
    where
      fn DoubleFlat   = Just doubleFlat
      fn Flat         = Just flat
      fn Nat          = Nothing
      fn Sharp        = Just sharp
      fn DoubleSharp  = Just doubleSharp 
      
  
instance LilyPondDuration Duration where
  quaternoteDuration  = quarter
  mkDuration d        = duration 4 -- to do

