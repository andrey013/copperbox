{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.Base.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Datatypes
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.Base.Datatypes
  (

  -- * Afm Unit
    AfmUnit
  , afmValue
  , afmUnitScale
  
  -- * Glyph metrics

  , PSCharCode
  , PSEncodingScheme
  , AfmBoundingBox

  , AfmKey
  , GlobalInfo
  , AfmFile(..)
  , AfmGlyphMetrics(..)

  , MonospaceDefaults(..)

  , FontProps(..)
  , buildMetricsOps
  
  ) where


import Wumpus.Basic.Kernel.Base.GlyphMetrics

import Wumpus.Core                              -- package: wumpus-core

import qualified Data.IntMap   as IntMap
import qualified Data.Map as Map



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

afmUnitScale :: AfmUnit -> PtSize 
afmUnitScale u = (realToFrac $ getAfmUnit u / 1000)


--------------------------------------------------------------------------------

-- | Afm files index glyphs by /PostScript character code/. This 
-- is not the same as Unicode, ASCII...
--
-- It is expected to be determined by @EncodingScheme@ in the
-- Global Font Information Section.
--
type PSCharCode         = Int

type PSEncodingScheme   = String

type AfmBoundingBox     = BoundingBox AfmUnit

type AfmKey         = String
type GlobalInfo     = Map.Map AfmKey String



-- | Wumpus needs a very small subset of AFM files, common to both
-- version 2.0 and version 4.1.
--
-- Note - Bounding Box is mandatory for AFM versions 3.0 and 4.1
-- 
-- Cap Height is optional in AFM versions 3.0 and 4.1. As Wumpus 
-- uses cap height in calculations, glyph metrics must be build 
-- with an arbitrary value if it is not present.
--
-- Encoding Scheme is optional in AFM files.
--
data AfmFile = AfmFile 
      { afm_encoding        :: Maybe String
      , afm_letter_bbox     :: Maybe AfmBoundingBox
      , afm_cap_height      :: Maybe AfmUnit
      , afm_glyph_metrics   :: [AfmGlyphMetrics]
      }
  deriving (Show) 
  
-- Note - for AfmFile BBox is a required field for version 4.1, 
-- but it appears to be optional for version 2.0.
--


data AfmGlyphMetrics = AfmGlyphMetrics
      { afm_char_code       :: !PSCharCode
      , afm_width_vector    :: !(Vec2 AfmUnit)
      , afm_char_name       :: !String
      }
  deriving (Eq,Show)


-- | Monospace defaults are used if the font loader fails to 
-- extract the necessary fields.
-- 
-- The values are taken from the font correpsonding to Courier 
-- in the distributed font files.
--
data MonospaceDefaults cu = MonospaceDefaults 
      { default_letter_bbox  :: BoundingBox cu
      , default_cap_height   :: cu
      , default_char_width   :: Vec2 cu
      }
  deriving (Eq,Show)



-- | The metrics read from a font file by a font loader. 
-- 
-- NOTE - FontProps is parametric on @cu@ - /Character Unit/ and 
-- not on the usual @u@. A typical character unit is 'AfmUnit', 
-- the unit of measurement for AFM files (1000th of a point).
--
-- The is the initial representation used by Wumpus-Basic as an
-- syntax tree when loading font files. 
--
data FontProps cu = FontProps
       { fp_bounding_box        :: BoundingBox cu 
       , fp_default_adv_vec     :: Vec2 cu
       , fp_adv_vecs            :: IntMap.IntMap (Vec2 cu)
       , fp_cap_height          :: cu
       }


-- | Build a MetricsOps function table, from a character unit
-- scaling function and FontProps read from a file.
--
buildMetricsOps :: (cu -> PtSize) -> FontProps cu -> MetricsOps
buildMetricsOps fn (FontProps (BBox ll ur) (V2 vx vy) 
                              vec_table    cap_height) = 
    MetricsOps
      { get_bounding_box  = \sz -> BBox (scalePt sz ll) (scalePt sz ur)
      , get_cw_table      = \sz i -> 
            maybe (defaultAV sz) (scaleVec sz) $ IntMap.lookup i vec_table 
      , get_cap_height    = \sz -> upscale sz (fn cap_height)
      }
  where
    upscale sz d            = fromPtSize $ sz * d 
 
    defaultAV sz            = V2 (upscale sz $ fn vx) (upscale sz $ fn vy) 
    scalePt  sz (P2 cx cy)  = P2 (upscale sz $ fn cx) (upscale sz $ fn cy) 
    scaleVec sz (V2 cx cy)  = V2 (upscale sz $ fn cx) (upscale sz $ fn cy) 


