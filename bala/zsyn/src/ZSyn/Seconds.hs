{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSyn.Seconds
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Seconds datatypes.
--
--------------------------------------------------------------------------------


module ZSyn.Seconds
  (
    Seconds
  , secondsToSamples
  , sampleLength

  ) where

newtype Seconds = Seconds { getSeconds :: Double }
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show Seconds where
  showsPrec p d = showsPrec p (getSeconds d)



secondsToSamples :: Double -> Seconds -> Int
secondsToSamples sr dur = truncate (dur * realToFrac sr)


sampleLength :: Double -> Seconds -> Double
sampleLength sr dur = sr * realToFrac dur