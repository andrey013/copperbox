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
  , MetricsOps(..)
  , FontMetricsOps(..)

  , GlyphMetrics
  , emptyGlyphMetrics
  , lookupFont
  , insertFont

  , monospace_metrics

  
  ) where

import Wumpus.Core                              -- package: wumpus-core

import qualified Data.Map      as Map
import Data.Monoid



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



-- | Operations on the metrics set of a font.
--
-- The is the internal representation used by Wumpus-Basic after
-- parsing the font file.
--
data MetricsOps = MetricsOps
      { get_bounding_box  :: forall u. FromPtSize u => PtSize -> BoundingBox u 
      , get_cw_table      :: forall u. FromPtSize u => PtSize -> CharWidthTable u
      , get_cap_height    :: forall u. FromPtSize u => PtSize -> u
      , get_descender     :: forall u. FromPtSize u => PtSize -> u
      }

-- | 'MetricsOps' for a particular named font.
-- 
data FontMetricsOps = FontMetricsOps FontName MetricsOps


-- | A map between a font name and MetricsOps.
--
newtype GlyphMetrics = GlyphMetrics { 
          getGlyphMetrics :: Map.Map FontName MetricsOps }

instance Monoid GlyphMetrics where
  mempty        = emptyGlyphMetrics
  a `mappend` b = GlyphMetrics $ getGlyphMetrics a `mappend` getGlyphMetrics b


emptyGlyphMetrics :: GlyphMetrics
emptyGlyphMetrics = GlyphMetrics $ Map.empty

lookupFont :: FontName -> GlyphMetrics -> Maybe MetricsOps
lookupFont name = Map.lookup name . getGlyphMetrics

insertFont :: FontMetricsOps -> GlyphMetrics -> GlyphMetrics
insertFont (FontMetricsOps name ops) = 
    GlyphMetrics . Map.insert name ops . getGlyphMetrics

-- | This ignores the Char code lookup and just returns the 
-- default advance vector.
--
monospace_metrics :: MetricsOps
monospace_metrics = MetricsOps
    { get_bounding_box  = \sz -> BBox (lowerLeft sz) (upperRight sz)
    , get_cw_table      = \sz _ -> hvec (upscale sz width_vec) 
    , get_cap_height    = \sz -> upscale sz cap_height
    , get_descender     = \sz -> upscale sz descender
    }
  where
    llx             = (-23)  / 1000
    lly             = (-250) / 1000
    urx             = 715    / 1000
    ury             = 805    / 1000
    width_vec       = 600    / 1000
    cap_height      = 562    / 1000
    descender       = (-157) / 1000

    upscale sz d    = fromPtSize $ sz * d
    lowerLeft sz    = P2 (upscale sz llx) (upscale sz lly) 
    upperRight sz   = P2 (upscale sz urx) (upscale sz ury) 


