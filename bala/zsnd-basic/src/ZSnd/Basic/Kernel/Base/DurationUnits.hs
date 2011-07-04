{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Base.DurationUnits
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Duration units.
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Base.DurationUnits
  ( 
    Seconds
  
  , RationalDuration
  , unwrapRationalDuration

  , FloatDuration

  ) where

import ZSnd.Basic.Kernel.Base.BaseDefs

import Data.VectorSpace                         -- package: vector-space

import Data.Ratio

-- | Wrapped Double representing seconds.
--
-- Note - this type is largely redundant. ZSnd uses @Double@ to
-- represent seconds.
-- 
newtype Seconds = Seconds { getSeconds :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show Seconds where
  showsPrec p d = showsPrec p (getSeconds d)


instance InterpretUnit Seconds where
  normalize _ a = getSeconds a
  dinterp _ d   = Seconds d



instance AdditiveGroup Seconds where
  zeroV = 0
  (^+^) = (+)
  negateV = negate

instance VectorSpace Seconds where
  type Scalar Seconds = Double
  (*^) s a = (Seconds s) * a



--------------------------------------------------------------------------------

newtype RationalDuration = RD { getRD :: Rational } 
  deriving (Enum,Eq,Fractional,Num,Ord,Real,RealFrac)



instance Show RationalDuration where
  showsPrec p = showsPrec p . getRD

instance AdditiveGroup RationalDuration where
  zeroV = 0
  (^+^) = (+)
  negateV = negate

instance VectorSpace RationalDuration where
  type Scalar RationalDuration = Rational
  (*^) s a = realToFrac s * a

instance InterpretUnit RationalDuration where
  normalize bpm a = let dwn_secs = 4.0 * (realToFrac $ 60 / bpm)
                    in realToFrac a * dwn_secs

  dinterp bpm d   = let dwn_secs = 4.0 * (realToFrac $ 60 / bpm)
                    in realToFrac d / dwn_secs



unwrapRationalDuration :: RationalDuration -> (Integer,Integer)
unwrapRationalDuration = prod numerator denominator . getRD
  where
    prod f g r = (f r, g r)



--------------------------------------------------------------------------------

-- | Context sensitive - 0.25 represents a quarter note which 
-- is scaled according to the current tempo (bpm).
--
-- Internally the represntation is a Double - i.e. a floating 
-- point type.
--
newtype FloatDuration = FD { getFD :: Double } 
  deriving (Enum,Eq,Fractional,Num,Ord,Real,RealFrac)



instance Show FloatDuration where
  showsPrec p = showsPrec p . getFD

instance AdditiveGroup FloatDuration where
  zeroV = 0
  (^+^) = (+)
  negateV = negate

instance VectorSpace FloatDuration where
  type Scalar FloatDuration = Double
  (*^) s a = realToFrac s * a

instance InterpretUnit FloatDuration where
  normalize bpm a = let dwn_secs = 4.0 * (realToFrac $ 60 / bpm)
                    in realToFrac a * dwn_secs

  dinterp bpm d   = let dwn_secs = 4.0 * (realToFrac $ 60 / bpm)
                    in realToFrac d / dwn_secs

