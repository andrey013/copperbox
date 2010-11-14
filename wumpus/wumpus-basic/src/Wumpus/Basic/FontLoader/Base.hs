{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.FontLoader.AfmV2
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM file parser.
--
-- Note - AFM Version 2.0 used by GhostScript and Version 3.0+
-- have numerous differences. 
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.FontLoader.Base
  (

  -- * Afm Unit
    AfmUnit
  , afmValue
  
  -- * Glyph metrics

  , PSCharCode
  , PSEncodingScheme
  , AfmBoundingBox
  , AfmGlyphMetrics(..)

  -- * Font loading
  , FontName
  , FontParseErr
  , FontParseResult
  , FontLoader(..)


  ) where

import Wumpus.Basic.Text.Datatypes              


import Wumpus.Core                              -- package: wumpus-core




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



-- | Afm files index glyphs by /PostScript character code/.
-- This is not the same as Unicode, ASCII...
--
-- It is expected to be determined by @EncodingScheme@ in the
-- Global Font Information Section.
--
type PSCharCode         = Int

type PSEncodingScheme   = String

type AfmBoundingBox     = BoundingBox AfmUnit

data AfmGlyphMetrics = AfmGlyphMetrics
      { afm_char_code       :: !PSCharCode
      , afm_width_vector    :: !(Vec2 AfmUnit)
      , afm_char_name       :: !String
      }
  deriving (Eq,Show)

-- Maybe the CharMetricsTable should be scaled to Wumpus units as 
-- the last part of the parsing process...

type FontName           = String
type FontParseErr       = String
type FontParseResult cu = Either FontParseErr (CharMetricsTable cu)

data FontLoader cu = FontLoader 
      { path_to_font_dir    :: FilePath
      , file_name_locator   :: FontName -> FilePath
      , font_parser         :: FilePath -> IO (FontParseResult cu)
      }

      


{-

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

-}



