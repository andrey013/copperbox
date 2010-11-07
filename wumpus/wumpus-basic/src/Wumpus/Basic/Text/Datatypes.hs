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
-- Datatypes for handling font metrics.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Text.Datatypes
  ( 
  
    CodePoint

  -- * AFM file measurement unit
  , AfmUnit
  , afmUnit

  , afmValue

  , CharBoundingBox
  , charBoundingBox
  , destCharBoundingBox

  , CharGeometry
  , CharMetricsTable(..)


  ) where

import Wumpus.Core                              -- package: wumpus-core


import qualified Data.Map as Map

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
afmValue :: FromPtSize u => AfmUnit -> PtSize -> u
afmValue u pt = fromPtSize $ (realToFrac $ afmUnit u) * (pt / 1000)


-- | Character bounding boxes have different coordinates to 
-- the /normal/ bounding boxes in Wumpus. 
--
-- Also, they might tpically have a different unit to a Wumpus 
-- drawing.
-- 
newtype CharBoundingBox cu = CharBoundingBox { 
          getCharBoundingBox :: BoundingBox cu }
  deriving (Eq,Show)

charBoundingBox :: cu -> cu -> cu -> cu -> CharBoundingBox cu
charBoundingBox llx lly urx ury = 
    CharBoundingBox $ BBox (P2 llx lly) (P2 urx ury)

destCharBoundingBox :: CharBoundingBox cu -> (cu,cu,cu,cu)
destCharBoundingBox = destBoundingBox . getCharBoundingBox


type CharGeometry cu = (CharBoundingBox cu, Vec2 cu)

data CharMetricsTable cu = CharMetricsTable
       { default_geom   :: CharGeometry cu
       , char_geoms     :: Map.Map CodePoint (CharGeometry cu) 
       }

