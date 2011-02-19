{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Units
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Numeric type representing PostScript points (1/72 inch) which is 
-- PostScript and Wumpus-Core\'s internal unit size.
--
-- Type class for converting other units to Postscript points for 
-- rendering. Other unit types (e.g. centimeter) should define an 
-- appropriate instance of PtSize.
-- 
-- Note - implicitly Wumpus-Core treats @Double@ as representing
-- PostScript point size / unit.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.Units
  ( 
  
  -- * PostScript point size type
    PsPoint
  
  -- * Extract (unscaled) PtSize as a Double 
  , ptSize



  -- * Centimeter type
  , Centimeter   
  , cm

  -- * Pica type
  , Pica
  , pica

  -- * Conversion class
  , PtSize(..)

  -- * Convert a Double
  , dpoint

  , pspt
  , psptFmt

  ) where

import Wumpus.Core.Utils.Common
import Wumpus.Core.Utils.FormatCombinators

import Data.Ratio

-- | Wrapped Double representing /Point size/ for font metrics 
-- etc.
-- 
newtype PsPoint = PsPoint 
          { psPoint :: Double  -- ^ Extract Point Size as a Double 
          } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show PsPoint where
  showsPrec p d = showsPrec p (psPoint d)

instance Format PsPoint where
  format = psptFmt

-- | Extract a point size as a @Double@.
--
ptSize :: PsPoint -> Double
ptSize = psPoint


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


instance Format Centimeter where
  format c = dtruncFmt (getCentimeter c) <> text "cm"

                            
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
  format p = dtruncFmt (getPica p) <> text "pica"
                            
pica :: Fractional u => Pica -> u 
pica = realToFrac . (* 12.0) . getPica

--------------------------------------------------------------------------------


-- | Convert units to and from PsPoint scaling accordingly.
--
-- Note - the instances for the Haskell built-in types perfom no 
-- scaling. To Wumpus @Double@ is synonymous with PostScript 
-- points, hence Haskell numeric types that map to Double are not 
-- scaled.
--
-- Note - instances for the integral types, Int and Integer, use 
-- @floor@ for the conversion @fromPsPoint@. Hence they are not 
-- numerically accurate. They are provided for convenience only.
-- 
class Num u => PtSize u where
  fromPsPoint :: PsPoint -> u
  toPsPoint  :: u -> PsPoint

instance PtSize Double where
  fromPsPoint = psPoint
  toPsPoint   = PsPoint
  
instance PtSize Float where
  fromPsPoint = realToFrac . psPoint
  toPsPoint   = PsPoint . realToFrac


instance PtSize Int where
  fromPsPoint = floor . psPoint
  toPsPoint   = PsPoint . fromIntegral

instance PtSize Integer where
  fromPsPoint = floor . psPoint
  toPsPoint   = PsPoint . fromIntegral

instance PtSize (Ratio Integer) where
  fromPsPoint = realToFrac
  toPsPoint   = realToFrac

instance PtSize (Ratio Int) where
  fromPsPoint = realToFrac
  toPsPoint   = realToFrac


instance PtSize PsPoint where
  fromPsPoint = id
  toPsPoint   = id




-- | By convention Wumpus uses the standard Haskell @Double@ as
-- point size.
-- 
-- This function casts a Double to another unit (e.g. @pica@) that
-- supports @PtSize@.
--
dpoint :: PtSize u => Double -> u
dpoint = fromPsPoint . toPsPoint



-- | Format a value as truncated double representing PostScript 
-- points.
--
pspt :: PtSize u => u -> String
pspt = truncateDouble . psPoint . toPsPoint

-- | Version of 'pspt' for Wumpus-Core\'s internal pretty printer.
--
psptFmt :: PtSize u => u -> Doc
psptFmt = dtruncFmt . psPoint . toPsPoint
