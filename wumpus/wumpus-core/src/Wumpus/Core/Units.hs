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

  -- * Centimeter type
    Centimeter   
  , cm

  -- * Pica type
  , Pica
  , pica

  -- * Conversion class
  , PsDouble(..)


  , pspt
  , psptFmt

  ) where

import Wumpus.Core.Utils.Common
import Wumpus.Core.Utils.FormatCombinators

import Data.Ratio




--------------------------------------------------------------------------------

-- | Wrapped Double /Centimeter/ unit type.
-- 
newtype Centimeter = Centimeter { getCentimeter :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)


instance Show Centimeter where
  showsPrec p d = showsPrec p (getCentimeter d)

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
class (Num u, Ord u) => PsDouble u where
  fromPsDouble :: Double -> u
  toPsDouble   :: u -> Double

instance PsDouble Double where
  fromPsDouble = id
  toPsDouble   = id
  
instance PsDouble Float where
  fromPsDouble = realToFrac
  toPsDouble   = realToFrac


instance PsDouble Int where
  fromPsDouble = floor
  toPsDouble   = fromIntegral

instance PsDouble Integer where
  fromPsDouble = floor
  toPsDouble   = fromIntegral

instance PsDouble (Ratio Integer) where
  fromPsDouble = realToFrac
  toPsDouble   = realToFrac

instance PsDouble (Ratio Int) where
  fromPsDouble = realToFrac
  toPsDouble   = realToFrac


instance PsDouble Centimeter where
  fromPsDouble = Centimeter . (0.03514598 *)
  toPsDouble   = cm

instance PsDouble Pica where
  fromPsDouble = Pica . (\x -> x / 12.0)
  toPsDouble   = pica


-- | Format a value as truncated double representing PostScript 
-- points.
--
pspt :: PsDouble u => u -> String
pspt = truncateDouble . toPsDouble

-- | Version of 'pspt' for Wumpus-Core\'s internal pretty printer.
--
psptFmt :: PsDouble u => u -> Doc
psptFmt = dtruncFmt . toPsDouble
