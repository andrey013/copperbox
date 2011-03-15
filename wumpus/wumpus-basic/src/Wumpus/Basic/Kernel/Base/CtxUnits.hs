{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.CtxUnits
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- \"Contextual\" units - @em@, @en@ and @AfmUnit@.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.CtxUnits
  (

    Em
  , En

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs


--------------------------------------------------------------------------------




-- | Wrapped Double representing an Em. 
-- 
newtype Em = Em { getEm :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show Em where
  showsPrec p d = showsPrec p (getEm d)

instance InterpretUnit Em where
  normalize sz a = fromIntegral sz * realToFrac a
  dinterp sz d   = realToFrac d / fromIntegral sz



-- | Wrapped Double representing an En.
-- 
newtype En = En { getEn :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show En where
  showsPrec p d = showsPrec p (getEn d)


instance InterpretUnit En where
  normalize sz a = (realToFrac  a) * 0.5 * fromIntegral sz
  dinterp sz d   = 2 * (realToFrac d) / (fromIntegral sz)



instance LengthTolerance Em         where length_tolerance = 0.01
instance LengthTolerance En         where length_tolerance = 0.01
