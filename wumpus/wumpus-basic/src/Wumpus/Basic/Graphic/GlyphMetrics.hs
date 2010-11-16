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
       { glyph_max_height   :: cu 
       , default_adv_vec    :: Vec2 cu
       , glyph_adv_vecs     :: IntMap.IntMap (Vec2 cu)
       }


-- unit_scale_fun     :: PtSize -> cu -> u
--

-- afmValue  :: FromPtSize u => AfmUnit -> PtSize -> u
--

data GlyphMetrics = GlyphMetrics 
      { get_max_height :: forall u. FromPtSize u => PtSize -> u 
      , get_av_lookup  :: forall u. FromPtSize u => PtSize -> (CodePoint -> Vec2 u)
      }


type BaseGlyphMetrics = Map.Map FontName GlyphMetrics

-- | This ignores the Char code lookup and just returns the 
-- default advance vector.
--
monospace_metrics :: GlyphMetrics
monospace_metrics = GlyphMetrics
      { get_max_height  = \sz -> fromPtSize $ sz * max_height
      , get_av_lookup   = \sz _ -> hvec (fromPtSize $ sz * width_vec) 
      }
  where
    max_height = 1055 / 1000
    width_vec  = 600 / 1000


buildMetrics :: (cu -> PtSize) -> GlyphMetricsTable cu -> GlyphMetrics
buildMetrics fn (GlyphMetricsTable max_height (V2 vx vy) vec_table) = 
    GlyphMetrics
      { get_max_height  = \sz -> ptScale sz $ fn max_height
      , get_av_lookup   = \sz i -> 
            maybe (defaultAV sz) (scaleFun sz) $ IntMap.lookup i vec_table 
      }
  where
    ptScale sz d            = fromPtSize $ sz * d 
 
    defaultAV sz            = V2 (ptScale sz $ fn vx) (ptScale sz $ fn vy) 
    scaleFun sz (V2 cx cy)  = V2 (ptScale sz $ fn cx) (ptScale sz $ fn cy) 



