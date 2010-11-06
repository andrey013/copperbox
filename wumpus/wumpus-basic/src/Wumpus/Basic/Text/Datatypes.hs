{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Text.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- AfmUnit - a numeric type representing the unit of measurement in 
-- AFM font specification files.
--
 
--------------------------------------------------------------------------------

module Wumpus.Basic.Text.Datatypes
  ( 
  
    CodePoint

  -- * AFM file measurement unit
  , AfmUnit
  , afmUnit

  , afmValue

  ) where


import Wumpus.Core                              -- package: wumpus-core


-- | a Unicode code-point.
--
type CodePoint = Int


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


