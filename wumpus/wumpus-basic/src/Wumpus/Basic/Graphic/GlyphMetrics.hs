{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.GlyphMetrics
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

module Wumpus.Basic.Graphic.GlyphMetrics
  ( 

    FontName
  , CodePoint
  , GlyphMetricsTable(..)
  , GlyphMetrics(..)
  , buildMetrics
  , BaseGlyphMetrics
  , monospace_metrics

  
  ) where

import Wumpus.Core                              -- package: wumpus-core

import qualified Data.IntMap   as IntMap
import qualified Data.Map      as Map

type FontName = String

-- | A Unicode code-point.
--
type CodePoint = Int


-- | NOTE - GlyphMetrics table is parametric on @cu@ - 
-- /Character unit/ and not on the usual @u@.
--
data GlyphMetricsTable cu = GlyphMetricsTable
       { glyph_bounding_box     :: BoundingBox cu 
       , default_adv_vec        :: Vec2 cu
       , glyph_adv_vecs         :: IntMap.IntMap (Vec2 cu)
       }


-- unit_scale_fun     :: PtSize -> cu -> u
--

-- afmValue  :: FromPtSize u => AfmUnit -> PtSize -> u
--

data GlyphMetrics = GlyphMetrics 
      { get_bounding_box  :: forall u. FromPtSize u => PtSize -> BoundingBox u 
      , get_av_lookup     :: forall u. FromPtSize u => PtSize -> (CodePoint -> Vec2 u)
      }


type BaseGlyphMetrics = Map.Map FontName GlyphMetrics

-- | This ignores the Char code lookup and just returns the 
-- default advance vector.
--
monospace_metrics :: GlyphMetrics
monospace_metrics = GlyphMetrics
      { get_bounding_box  = \sz -> BBox (lowerLeft sz) (upperRight sz)
      , get_av_lookup     = \sz _ -> hvec (upscale sz width_vec) 
      }
  where
    llx           = (-23)  / 1000
    lly           = (-250) / 1000
    urx           = 715    / 1000
    ury           = 805    / 1000
    width_vec     = 600    / 1000

    upscale sz d  = fromPtSize $ sz * d
    lowerLeft sz  = P2 (upscale sz llx) (upscale sz lly) 
    upperRight sz = P2 (upscale sz urx) (upscale sz ury) 


buildMetrics :: (cu -> PtSize) -> GlyphMetricsTable cu -> GlyphMetrics

buildMetrics fn (GlyphMetricsTable (BBox ll ur) (V2 vx vy) vec_table) = 
    GlyphMetrics
      { get_bounding_box  = \sz -> BBox (scalePt sz ll) (scalePt sz ur)
      , get_av_lookup     = \sz i -> 
            maybe (defaultAV sz) (scaleVec sz) $ IntMap.lookup i vec_table 
      }
  where
    upscale sz d            = fromPtSize $ sz * d 
 
    defaultAV sz            = V2 (upscale sz $ fn vx) (upscale sz $ fn vy) 
    scalePt  sz (P2 cx cy)  = P2 (upscale sz $ fn cx) (upscale sz $ fn cy) 
    scaleVec sz (V2 cx cy)  = V2 (upscale sz $ fn cx) (upscale sz $ fn cy) 



