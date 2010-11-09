{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
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

  , afmValue

  , AdvanceVec
  , advanceH

  , CharBoundingBox
  , charBoundingBox
  , destCharBoundingBox

  , CharMetricsTable(..)
  , AfmCharMetricsTable

  ) where

import Wumpus.Core                              -- package: wumpus-core


import qualified Data.IntMap as IntMap

-- | a Unicode code-point.
--
type CodePoint = Int


-- | Wrapped Double representing 1\/1000 of the scale factor
-- (Point size) of a font. AFM files encode all measurements 
-- as these units. 
-- 
newtype AfmUnit = AfmUnit { getAfmUnit :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show AfmUnit where
  showsPrec p d = showsPrec p (getAfmUnit d)


-- | Compute the size of a measurement in Afm units scaled by the
-- point size of the font.
--
afmValue :: FromPtSize u => AfmUnit -> PtSize -> u
afmValue u pt = fromPtSize $ (realToFrac $ getAfmUnit u) * (pt / 1000)




type AdvanceVec u = Vec2 u

-- | Take the max width and set the height to zero.
--
-- It is assumed that any deviation from zero in the height
-- component represents that the end vector is in super- or 
-- sub-script mode. As 'advanceHMax' is used in multi-line 
-- concatenation, losing the mode seems acceptable.
--
advanceH :: Num u => AdvanceVec u -> Vec2 u
advanceH (V2 w _)  = V2 w 0





-- | Character bounding boxes have different coordinates to 
-- the /normal/ bounding boxes in Wumpus. 
--
-- Also, they might typically have a different unit to a Wumpus 
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



data CharMetricsTable cu = CharMetricsTable
       { glyph_max_height   :: cu 
       , default_adv_vec    :: Vec2 cu
       , char_adv_vecs      :: IntMap.IntMap (Vec2 cu)
       }

type AfmCharMetricsTable = CharMetricsTable AfmUnit


-- Note - the bounding box of a glyph is seemingly _not_ useful 
-- for calculating the bound box of a string. For instance, 
-- /space/ has a zero-width, zero-height bounding box as it has no
-- content, however when drawn it does take up physical space.
-- The size of this space is determined by its advance vector.
--
-- Height can be extracted from the Font bounding box.
--
-- Note the Font bounding box is very wide to use as a default 
-- width...
--


