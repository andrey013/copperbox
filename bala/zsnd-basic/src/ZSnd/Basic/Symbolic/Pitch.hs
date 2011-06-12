{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Symbolic.Pitch
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- 
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Symbolic.Pitch
  ( 

    PitchC
  , pitchC

  , pcMidi
  , pcHz

  ) where

-- Note - there is value to avoiding pitch (and interval) naming
-- as we are only generating numerical values...


import ZSnd.Basic.Symbolic.Z12


-- | Pitch-class representation - octave and pitch-class
-- 
data PitchC = PitchC Int Z12
  deriving (Eq,Ord)

instance Show PitchC where
  showsPrec _ (PitchC o i) = shows o . showChar '.' . shows i
 

pitchC :: Int -> Z12 -> PitchC
pitchC o i = PitchC o i


pcMidi :: PitchC -> Int
pcMidi (PitchC o i) = 60 + (12*(o-8)) + fromIntegral i



pcHz :: PitchC -> Double
pcHz (PitchC o i) = 
    (2.0 ** (od + (0.08333333 * (fromIntegral i)))) * 1.021975
  where
    od = fromIntegral o

