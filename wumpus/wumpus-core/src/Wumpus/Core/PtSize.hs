{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PtSize
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

module Wumpus.Core.PtSize
  ( 
  
  -- * Point size type
    PsPoint
  
  -- * Extract (unscaled) PtSize as a Double 
  , ptSize

  -- * Conversion class
  , PtSize(..)

  , pspt
  , psptFmt

  ) where

import Wumpus.Core.Utils.Common
import qualified Wumpus.Core.Utils.FormatCombinators as Fmt

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

-- | Extract a point size as a @Double@.
--
ptSize :: PsPoint -> Double
ptSize = psPoint




{-
-- | Conversion to to PostScript point units for printing.
--
-- Instances must define at least @toDouble@.
--
class Num a => PSUnit a where
  toDouble :: a -> Double
  dtrunc   :: a -> String
  
  dtrunc = truncateDouble . toDouble

instance PSUnit Double where
  toDouble = id
  dtrunc   = truncateDouble

instance PSUnit Float where
  toDouble = realToFrac

instance PSUnit (Ratio Integer) where
  toDouble = realToFrac

instance PSUnit (Ratio Int) where
  toDouble = realToFrac
-}


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


-- | Format a value as truncated double representing PostScript 
-- points.
--
pspt :: PtSize u => u -> String
pspt = truncateDouble . psPoint . toPsPoint

-- | Version of 'pspt' for Wumpus-Core\'s internal pretty printer.
--
psptFmt :: PtSize u => u -> Fmt.Doc
psptFmt = dtruncFmt . psPoint . toPsPoint
