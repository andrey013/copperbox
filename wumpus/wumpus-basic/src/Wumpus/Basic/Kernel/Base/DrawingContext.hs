{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.DrawingContext
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Drawing attributes
--
-- \*\* WARNING \*\* - this module needs systematic naming 
-- schemes both for update functions (primaryColour, ...) and 
-- for synthesized selectors (e.g. lowerxHeight). The current 
-- names will change.
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.DrawingContext
  ( 

  -- * Drawing context
    DrawingContext(..)
  , DrawingContextF

  , standardContext
  , metricsContext

  , default_drawing_context

  -- * DrawingCtxM (reader) monad
  , DrawingCtxM(..)
  , asksDC


  -- * Glyph metrics
  , withFontMetrics

  
  ) where


import Wumpus.Basic.Kernel.Base.GlyphMetrics

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.StandardEncoding

import Control.Applicative
import Data.Maybe


data DrawingContext = DrawingContext
      { glyph_tables          :: GlyphMetrics
      , fallback_metrics      :: MetricsOps
      , stroke_props          :: StrokeAttr
      , font_props            :: FontAttr
      , stroke_colour         :: RGBi      -- also text colour...
      , fill_colour           :: RGBi      
      , line_spacing_factor   :: Double
      }

-- TODO - hand craft a Show instance 

type DrawingContextF = DrawingContext -> DrawingContext


standardContext :: FontSize -> DrawingContext
standardContext sz = 
    DrawingContext { glyph_tables         = emptyGlyphMetrics
                   , fallback_metrics     = monospace_metrics
                   , stroke_props         = default_stroke_attr
                   , font_props           = FontAttr sz wumpus_courier
                   , stroke_colour        = wumpus_black
                   , fill_colour          = wumpus_light_gray
                   , line_spacing_factor  = 1.2  
                   }

-- out-of-date - should be adding loaded fonts, not replacing the 
-- GlyphMetrics Map wholesale.
--
metricsContext :: FontSize -> GlyphMetrics -> DrawingContext
metricsContext sz bgm = 
    DrawingContext { glyph_tables         = bgm
                   , fallback_metrics     = monospace_metrics
                   , stroke_props         = default_stroke_attr
                   , font_props           = FontAttr sz wumpus_courier
                   , stroke_colour        = wumpus_black
                   , fill_colour          = wumpus_light_gray
                   , line_spacing_factor  = 1.2  
                   }




wumpus_black            :: RGBi
wumpus_black            = RGBi 0 0 0 

wumpus_light_gray       :: RGBi
wumpus_light_gray       = RGBi 200 200 200


-- | Courier
-- 
wumpus_courier :: FontFace
wumpus_courier = 
    FontFace "Courier" "Courier New" SVG_REGULAR standard_encoding



default_drawing_context :: DrawingContext
default_drawing_context = 
    standardContext (font_size wumpus_default_font)



--------------------------------------------------------------------------------


class (Applicative m, Monad m) => DrawingCtxM (m :: * -> *) where
  askDC    :: m DrawingContext
  localize :: (DrawingContext -> DrawingContext) -> m a -> m a


-- | Project a value out of a context.
--
asksDC :: DrawingCtxM m => (DrawingContext -> a) -> m a
asksDC f = askDC >>= (return . f)



--------------------------------------------------------------------------------
-- Glyph metrics

-- These are directly on the DrawingContext /for efficiency/.



withFontMetrics :: (MetricsOps -> PtSize -> u) -> DrawingContext -> u
withFontMetrics fn ctx@(DrawingContext { font_props = font_stats }) = 
      fn metric_set point_sz
  where 
    ps_name     = ps_font_name $ font_face font_stats
    point_sz    = fromIntegral $ font_size font_stats 
    metric_set  = fromMaybe (fallback_metrics ctx) $ 
                    lookupFont ps_name (glyph_tables ctx) 


