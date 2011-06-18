{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Symbolic.PitchClass
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

module ZSnd.Symbolic.PitchClass
  ( 

    PC
  , pitchC

  , pcMidi
  , pcHz
  , pcPC
  , Iv

  ) where

-- Note - this representation avoids pitch (and interval) naming
-- as we are only generating numerical values...


import ZSnd.Symbolic.Basis.Z12

import Data.AffineSpace
import Data.VectorSpace


-- | Pitch-class representation - octave and pitch-class
-- 
data PC = PC Int Z12
  deriving (Eq,Ord)

instance Show PC where
  showsPrec _ (PC o i) = shows o . showChar '.' . shows i
 

pitchC :: Int -> Z12 -> PC
pitchC o i = PC o i


pcMidi :: PC -> Int
pcMidi (PC o i) = 60 + (12*(o-8)) + fromIntegral i

pcPC :: PC -> Double
pcPC (PC o i) = od + (0.01 * fromIntegral i) 
  where
    od = fromIntegral o


pcHz :: PC -> Double
pcHz (PC o i) = 
    (2.0 ** (od + (0.08333333 * (fromIntegral i)))) * 1.021975
  where
    od = fromIntegral o

-- | Interval is a count of semitones.
--
newtype Iv = Iv { getInterval :: Int }
  deriving (Enum,Eq,Ord,Integral,Num,Real)

instance Show Iv where
  showsPrec p d = showsPrec p (getInterval d)

instance AdditiveGroup Iv where
  zeroV   = 0
  negateV = negate
  (^+^)   = (+) 


semitoneDiff :: PC -> PC -> Iv
semitoneDiff (PC o0 i0) (PC o1 i1) = idif + Iv odif
  where
    odif = o1 - o0
    idif = if i1 > i0 then fromIntegral (i1 - i0) 
                      else negate $ fromIntegral (i0 - i1)

semitoneAdd :: PC -> Iv -> PC
semitoneAdd (PC o i) (Iv a) = PC (o + o') (i + fromIntegral i')
  where
    (o', i') = a `divMod` 12

instance AffineSpace PC where
  type Diff PC = Iv
  (.-.) = semitoneDiff
  (.+^) = semitoneAdd