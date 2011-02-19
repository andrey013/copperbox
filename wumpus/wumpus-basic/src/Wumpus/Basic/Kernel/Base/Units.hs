{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.Units
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Centimeter unit type
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.Units
  ( 


  -- * Convert a Double
    dpoint

  -- * Centimeter type
  , Centimeter   
  , cm

  -- * Pica type
  , Pica
  , pica

  ) where


import Wumpus.Core                              -- package: wumpus-core


{-
-- | Note the Double instance performs no scaling. As per 
-- PostScript where the default unit-size is Point, Wumpus 
-- considers Haskell Doubles to be synonymous with Point.
--
class Num u => ToPtSize u where 
  toPtSize :: u -> PtSize


instance ToPtSize Double where
  toPtSize = realToFrac

instance ToPtSize PtSize where
  toPtSize = id
-}

--------------------------------------------------------------------------------

-- | By convention Wumpus uses the standard Haskell @Double@ as
-- point size.
-- 
-- This function casts a Double to another unit (e.g. @pica@) that
-- supports @PtSize@.
--
dpoint :: PtSize u => Double -> u
dpoint = fromPsPoint . toPsPoint

--------------------------------------------------------------------------------

-- | Wrapped Double /Centimeter/ unit type.
-- 
newtype Centimeter = Centimeter { getCentimeter :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show Centimeter where
  showsPrec p d = showsPrec p (getCentimeter d)


instance PtSize Centimeter where
  fromPsPoint = Centimeter . (0.03514598 *) . ptSize
  toPsPoint   = cm
                            
cm :: Fractional u => Centimeter -> u 
cm = realToFrac . (28.45275619 *) . getCentimeter


--------------------------------------------------------------------------------
-- Pica

-- | Wrapped Double /Pica/ unit type.
-- 
-- Pica is 12 Points.
--
newtype Pica = Pica { getPica :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show Pica where
  showsPrec p d = showsPrec p (getPica d)


instance PtSize Pica where
  fromPsPoint = Pica . (\x -> x / 12.0) . ptSize
  toPsPoint   = pica

instance Format Pica where
  format = format . toPsPoint
                            
pica :: Fractional u => Pica -> u 
pica = realToFrac . (* 12.0) . getPica
