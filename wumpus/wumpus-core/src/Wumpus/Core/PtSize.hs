{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PtSize
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Numeric type representing Point size (1/72 inch) which is 
-- PostScript and Wumpus-Core\'s internal unit size.
--
-- Plus, AfmUnit numeric type representing the unit of measurement in 
-- AFM font specification files.
--
-- Other unit types (e.g. centimeter) should define an 
-- appropriate instance of FromPtSize.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.PtSize
  ( 
  
  -- * Point size type
    PtSize
  
  -- * Extract (unscaled) PtSize as a Double 
  , ptSize

  -- * Conversion class
  , FromPtSize(..)

  -- * AFM file measurement unit
  , AfmUnit
  , afmUnit

  , afmValue

  ) where


-- | Wrapped Double representing /Point size/ for font metrics 
-- etc.
-- 
newtype PtSize = PtSize { ptSize :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show PtSize where
  showsPrec p d = showsPrec p (ptSize d)


-- | Convert the value of PtSize scaling accordingly.
--
-- Note - the Double instance perfoms no scaling, this
-- is because internally Wumpus-Core works in points.
-- 
class Num u => FromPtSize u where
  fromPtSize :: PtSize -> u

instance FromPtSize Double where
  fromPtSize = ptSize



-- | Wrapped Double representing 1\/1000 of the scale factor
-- (Point size) of a font. AFM files encode all measurements 
-- as these units. 
-- 
newtype AfmUnit = AfmUnit { afmUnit :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show AfmUnit where
  showsPrec p d = showsPrec p (afmUnit d)


-- | Compute the size of a measurement in Afm units scaled by the
-- point size of the font.
--
afmValue :: AfmUnit -> PtSize -> Double
afmValue u pt = afmUnit u * (ptSize pt / 1000)


