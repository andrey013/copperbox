--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.BalaLy
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Use datatypes from Bala.Base libraries to implement the LilyPond interfaces.
--
--------------------------------------------------------------------------------


module Bala.Perform.Bala.BalaLy where

import Bala.Base
import Bala.Format.Output.OutputLilyPond hiding (longa, breve)
import qualified Bala.Format.Output.OutputLilyPond as Ly
import Bala.Perform.LilyPond.Class

  
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
  mkDuration d 
      | d == longa                      = Just Ly.longa  
      | d == double_whole               = Just Ly.breve 
      | d == whole                      = Just $ duration 1
      | d == half                       = Just $ duration 2
      | d == quarter                    = Just $ duration 4
      | d == eighth                     = Just $ duration 8
      | d == sixteenth                  = Just $ duration 16
      | d == thirty_second              = Just $ duration 32
      | d == sixty_fourth               = Just $ duration 64
      | d == one_hundred_twenty_eighth  = Just $ duration 128
      | otherwise                       = Nothing

