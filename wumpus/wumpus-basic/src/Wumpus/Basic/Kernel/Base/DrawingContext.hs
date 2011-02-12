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
-- \*\* WARNING \*\* - The drawing context modules need systematic 
-- naming schemes both for update functions (primaryColour, ...) 
-- and for synthesized selectors (e.g. lowerxHeight). The current 
-- names in @QueryDC@ and @UpdateDC@ are expected to change.
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.DrawingContext
  ( 

  -- * Drawing context types
    DrawingContext(..)

  , DrawingContextF
  , TextMargin(..)

  -- * Construction
  , standardContext
  , metricsContext


  -- * DrawingCtxM (reader) monad
  , DrawingCtxM(..)
  , asksDC


  -- * Glyph metrics
  , withFontMetrics

  
  ) where


import Wumpus.Basic.Kernel.Base.FontMetrics
import Wumpus.Basic.Kernel.Base.Units

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.StandardEncoding

import Control.Applicative
import Data.Maybe
import Data.Monoid

-- | 'DrawingContext' - the \"graphics state\" of Wumpus-Basic. 
-- DrawingContext is operated on within a Reader monad rather than 
-- a State monad so \"updates\" are delineated within a @local@ 
-- operation (called @localize@ in Wumpus), rather than permanent
-- until overridden as per @set@ of a State monad.
-- 
-- Note - @round_corner_factor@ is only accounted for by some 
-- graphic objects (certain Path objects and Shapes in 
-- Wumpus-Drawing for instance). There many be many objects that 
-- ignore it and are drawn only with angular corners.
-- 
-- Also note - in contrast to most other drawing objects in 
-- Wumpus, none of the measurement values are parameteric - 
-- usually notated with the type variable @u@ in Wumpus. This is 
-- so Wumpus can (potentially) support different units e.g. 
-- centimeters rather than just Doubles (represening printers 
-- points), though adding support for other units has a very low 
-- priority.
-- 
data DrawingContext = DrawingContext
      { font_metrics_table    :: FontTable
      , font_load_log         :: FontLoadLog
      , fallback_metrics      :: FontMetrics
      , stroke_props          :: StrokeAttr
      , font_props            :: FontAttr
      , stroke_colour         :: RGBi      -- also text colour...
      , fill_colour           :: RGBi      
      , text_colour           :: RGBi
      , line_spacing_factor   :: Pt
      , round_corner_factor   :: Pt 
      , text_margin           :: TextMargin
      , snap_grid_factors     :: (Pt,Pt)
      }

-- TODO - what parts of the Drawing Context should be strict? 


-- | Type synonym for DrawingContext update functions.
--
type DrawingContextF = DrawingContext -> DrawingContext

-- | The unit of Margin is always Double representing Points, e.g.
-- 1.0 is 1 Point. Margins are not scaled relative to the current
-- font size.
-- 
-- The default value is 2 point.
--
data TextMargin = TextMargin
       { text_margin_x          :: !Pt
       , text_margin_y          :: !Pt
       }


-- | 'standardContext' : @ font_size -> DrawingContext @  
--
-- Create a 'DrawingContext'.
-- 
-- Note - @font_size@ is used for sizing more than just text 
-- labels. Arrowheads, plot marks and other elements have their
-- metrics derived from the font size.
-- 
-- No real font metrics are present in the 'DrawingContext' 
-- created by 'standardContext'. Static, hard-coded fallback 
-- metrics derived from the @Courier@ font are available but
-- these metrics might not accurately correspond to the 
-- @Courier@ available to the the final renderer (GhostScript,
-- an SVG viewer, etc.).
-- 
-- Use this constructor for drawings that make primitive use of
-- text.
-- 
standardContext :: FontSize -> DrawingContext
standardContext sz = 
    DrawingContext { font_metrics_table   = emptyFontTable
                   , font_load_log        = mempty
                   , fallback_metrics     = monospace_metrics
                   , stroke_props         = default_stroke_attr
                   , font_props           = FontAttr sz wumpus_courier
                   , stroke_colour        = wumpus_black
                   , fill_colour          = wumpus_light_gray
                   , text_colour          = wumpus_black
                   , line_spacing_factor  = 1.2  
                   , round_corner_factor  = 0
                   , text_margin          = default_text_margin
                   , snap_grid_factors    = (50.0, 50.0)
                   }



-- | 'metricsContext' : @ font_size * font_metrics -> DrawingContext @  
--
-- Create a 'DrawingContext' with font metrics loaded from the 
-- file system.
-- 
-- Note - @font_size@ is used for sizing more than just text 
-- labels. Arrowheads, plot marks and other elements have their
-- metrics derived from the font size.
-- 
-- Use this constructor for drawings that make use of the text 
-- objects provided by @Wumpus-Drawing@ (DocText and RotText).
-- 
metricsContext :: FontSize -> FontLoadResult -> DrawingContext
metricsContext sz flZ = 
    let env = standardContext sz 
    in env { font_metrics_table = loaded_glyph_metrics flZ
           , font_load_log      = loader_errors flZ
           }


-- Helpers - not exported

default_text_margin :: TextMargin
default_text_margin = TextMargin { text_margin_x = 2.0, text_margin_y = 2.0 }


wumpus_black            :: RGBi
wumpus_black            = RGBi 0 0 0 

wumpus_light_gray       :: RGBi
wumpus_light_gray       = RGBi 200 200 200


-- | Courier
-- 
wumpus_courier :: FontFace
wumpus_courier = 
    FontFace "Courier" "Courier New" SVG_REGULAR standard_encoding


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



withFontMetrics :: (FontMetrics -> PtSize -> u) -> DrawingContext -> u
withFontMetrics fn ctx@(DrawingContext { font_props = font_stats }) = 
      fn metric_set point_sz
  where 
    ps_name     = ps_font_name $ font_face font_stats
    point_sz    = fromIntegral $ font_size font_stats 
    metric_set  = fromMaybe (fallback_metrics ctx) $ 
                    lookupFont ps_name (font_metrics_table ctx)


