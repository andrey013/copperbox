{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.Units
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Units @cm@, @pica@ and \"contextual\" units - @em@, @en@.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.Units
  (

    Centimeter
  , Pica
  , Em
  , En

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs


--------------------------------------------------------------------------------

-- | Wrapped Double representing Centimeter.
-- 
newtype Centimeter = Centimeter { getCentimeter :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)


instance Show Centimeter where
  showsPrec p d = showsPrec p (getCentimeter d)

dcm :: Double -> Centimeter
dcm = Centimeter . (0.03514598 *)
                            
cm :: Fractional u => Centimeter -> u 
cm = realToFrac . (28.45275619 *) . getCentimeter

instance InterpretUnit Centimeter where
  normalize _ = cm 
  dinterp   _ = dcm


-- | Wrapped Double /Pica/ unit type.
-- 
-- Pica is 12 Points.
--
newtype Pica = Pica { getPica :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)


instance Show Pica where
  showsPrec p d = showsPrec p (getPica d)

                            
pica :: Fractional u => Pica -> u 
pica = realToFrac . (* 12.0) . getPica

dpica :: Double -> Pica
dpica = Pica . (\x -> x / 12.0)

instance InterpretUnit Pica where
  normalize _ = pica
  dinterp   _ = dpica



--------------------------------------------------------------------------------
-- Contextual units

-- | Wrapped Double representing an Em. 
-- 
newtype Em = Em { getEm :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show Em where
  showsPrec p d = showsPrec p (getEm d)

instance InterpretUnit Em where
  normalize sz a = fromIntegral sz * realToFrac a
  dinterp sz d   = realToFrac d / fromIntegral sz

instance LengthTolerance Centimeter where length_tolerance = 0.01


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
