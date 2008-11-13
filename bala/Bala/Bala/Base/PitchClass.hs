{-# LANGUAGE FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.PitchClass
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Operations on pitch classes
--
--------------------------------------------------------------------------------


module Bala.Base.PitchClass where

import Bala.Base.Pitch

import Numeric (showInt)


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------


-- | Int must be between 1 & 12
newtype PC = PC {unPC :: Int}

type PCSet = [PC]



--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | pitch displacement relative to middle C (which is 0)
fixedPitch :: Pitch -> Int
fixedPitch = unPC . pc

pitchClass i | i >= 0 && i <= 11 = PC i
             | otherwise         = PC $ i `mod` 12

instance Show PC where
  showsPrec _ (PC i)         
      | i > 11 || i < 0 = showString "Error"
      | i == 11         = showChar 'E'
      | i == 10         = showChar 'T'
      | otherwise       = showInt i

class PitchClass a where pc :: a -> PC

instance PitchClass Pitch where
  pc p = pitchClass $ (12 * pch_octave p) + semitones p

{-
  
class Transpose a where transpose :: Int -> a -> a

instance Transpose PC where
  transpose i (PC n) = PC $ (i + n) `mod` 12

instance Transpose [PC] where
  transpose i ps = map (transpose i) ps
-}   