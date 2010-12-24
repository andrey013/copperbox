{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.GlyphMetrics
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data types representing glyph metrics loaded from font files.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.GlyphMetrics
  ( 

    FontName
  , CodePoint
  , CharWidthTable
  , FontProps(..)
  , MetricsOps(..)
  , buildMetricsOps
  , FontCalcs(..)

  , GlyphMetrics
  , emptyGlyphMetrics
  , lookupFont
  , insertFont

  , monospace_metrics

  
  ) where

import Wumpus.Core                              -- package: wumpus-core

import qualified Data.IntMap   as IntMap
import qualified Data.Map      as Map

type FontName = String

-- | A Unicode code-point.
--
type CodePoint = Int

-- | A lookup from code point to /width vector/.
--
-- Note - in PostScript terminology a width vector is not obliged
-- to be left-to-right (writing direction 0). It could be 
-- top-to-bottom (writing direction 1).
--
type CharWidthTable u = CodePoint -> Vec2 u


-- | The metrics read from a font file by a font loader. 
-- 
-- NOTE - FontProps is parametric on @cu@ - /Character unit/ and 
-- not on the usual @u@. A typical character is 'AfmUnit', the 
-- unit of measurement for AFM files (1000th of a point).
--
data FontProps cu = FontProps
       { fp_bounding_box        :: BoundingBox cu 
       , fp_default_adv_vec     :: Vec2 cu
       , fp_adv_vecs            :: IntMap.IntMap (Vec2 cu)
       , fp_cap_height          :: cu
       }


-- | Operations on the metrics set of a font.
--
data MetricsOps = MetricsOps
      { get_bounding_box  :: forall u. FromPtSize u => PtSize -> BoundingBox u 
      , get_cw_table      :: forall u. FromPtSize u => PtSize -> CharWidthTable u
      , get_cap_height    :: forall u. FromPtSize u => PtSize -> u
      }

-- Name for (FontName, MetricOps)... ?

data FontCalcs = FontCalcs FontName MetricsOps



newtype GlyphMetrics = GlyphMetrics { 
          getGlyphMetrics :: Map.Map FontName MetricsOps }

emptyGlyphMetrics :: GlyphMetrics
emptyGlyphMetrics = GlyphMetrics $ Map.empty

lookupFont :: FontName -> GlyphMetrics -> Maybe MetricsOps
lookupFont name = Map.lookup name . getGlyphMetrics

insertFont :: FontCalcs -> GlyphMetrics -> GlyphMetrics
insertFont (FontCalcs name ops) = 
    GlyphMetrics . Map.insert name ops . getGlyphMetrics

-- | This ignores the Char code lookup and just returns the 
-- default advance vector.
--
monospace_metrics :: MetricsOps
monospace_metrics = MetricsOps
      { get_bounding_box  = \sz -> BBox (lowerLeft sz) (upperRight sz)
      , get_cw_table      = \sz _ -> hvec (upscale sz width_vec) 
      , get_cap_height    = \sz -> upscale sz cap_height
      }
  where
    llx           = (-23)  / 1000
    lly           = (-250) / 1000
    urx           = 715    / 1000
    ury           = 805    / 1000
    width_vec     = 600    / 1000
    cap_height    = 562    / 1000

    upscale sz d  = fromPtSize $ sz * d
    lowerLeft sz  = P2 (upscale sz llx) (upscale sz lly) 
    upperRight sz = P2 (upscale sz urx) (upscale sz ury) 


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



