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
  -- * Alias for PtSize
    Pt

  -- * Type class ToPtSize
  , ToPtSize(..)

  -- * Centimeter type
  , Centimeter   
  , cm

  -- * Pica type
  , Pica
  , pica

  ) where


import Wumpus.Core                              -- package: wumpus-core

-- | To dis-ambiguate from geometric points, Wumpus-Core gives the 
-- Point (unit) type a rather cumbersome name. Wumpus-Basic adds a
-- useful alias.
--
type Pt = PtSize

-- | Note the Double instance performs no scaling. As per 
-- PostScript where the default unit-size is Point, Wumpus 
-- considers Haskell Doubles to be synonymous with Point.
--
class Num u => ToPtSize u where 
  toPtSize :: u -> PtSize


instance ToPtSize Double where
  toPtSize = realToFrac

-- | Wrapped Double /Centimeter/ unit type.
-- 
newtype Centimeter = Centimeter { getCentimeter :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show Centimeter where
  showsPrec p d = showsPrec p (getCentimeter d)


instance FromPtSize Centimeter where
  fromPtSize = Centimeter . (0.03514598 *) . ptSize

instance ToPtSize Centimeter where
  toPtSize = cm
                            
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


instance FromPtSize Pica where
  fromPtSize = Pica . ((/) 12.0) . ptSize

instance ToPtSize Pica where
  toPtSize = pica
                            
pica :: Fractional u => Pica -> u 
pica = realToFrac . (12 *) . getPica
