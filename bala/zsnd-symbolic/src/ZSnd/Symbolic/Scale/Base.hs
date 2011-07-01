{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Symbolic.Scale.Base
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Kraig Grady\'s 12-tone Centaur scale.
--
--------------------------------------------------------------------------------

module ZSnd.Symbolic.Scale.Base
  ( 

    HertzValue(..)
  , octaveToHertz

  , FromFrac(..)

  , toCents
  , fromCents


  -- * Interval 
  , Iv

  ) where


import ZSnd.Basic.Kernel                        -- package: zsnd-basic

import Data.VectorSpace                         -- package: vector-space
       

import Control.Applicative

-- Note - this representation avoids pitch (and interval) naming
-- as we are only generating numerical values...

-- | Convert a scale tone into hartz representation.
--
-- The conversion is expected to use @global_tuning@ from the 
-- Context.
--
class HertzValue a where
  hertz :: ContextM m => a -> m Double

-- | Converting to @octave@ representation then calling 
-- @octaveToHertz@ is potentailly the easiest way to implement the
-- @hertz@ method from @HertzValue @.
-- 
octaveToHertz :: ContextM m => Double -> m Double
octaveToHertz x = (\tv -> tv * 2.0 ** (x - 8.75)) <$> get_global_tuning


-- | The input should be interpreted as pitch-class notation:

-- > 8.00 = middle C for 12ET
-- > 8.01 = C# above middle C for 12ET
-- > ..
-- > 8.11 = B above middle C for 12ET
-- > 8.12 == 9.00 = C 1 octave above middle C in 12ET
--
class FromFrac a where
  fromFrac :: Double -> a 


-- | Note - uses @round@.
toCents :: Rational -> Int
toCents r = roundd $ 1200.0 * (logBase 2 $ fromRational r)


fromCents :: Int -> Double
fromCents c = 2 ** ((fromIntegral c) / 1200)


roundd :: Double -> Int
roundd = round





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
