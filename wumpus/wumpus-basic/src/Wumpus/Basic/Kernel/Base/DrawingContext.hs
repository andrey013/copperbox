{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.DrawingContext
-- Copyright   :  (c) Stephen Tetley 2010-2011
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
  , addFontTables

  , reset_drawing_properties
  , reset_drawing_metrics

  -- * DrawingCtxM (reader) monad
  , DrawingCtxM(..)
  , query


  -- * Glyph metrics
  , withFontMetrics

  
  ) where


import Wumpus.Basic.Kernel.Base.FontMetrics

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
      { dc_font_metrics_table   :: FontTable
      , dc_font_load_log        :: FontLoadLog
      , dc_fallback_metrics     :: FontMetrics
      , dc_font_face            :: FontFace
      , dc_font_size            :: !Int
      , dc_snap_grid_factors    :: (Double,Double)
      , dc_stroke_props         :: StrokeAttr
      , dc_stroke_colour        :: RGBi      -- also text colour...
      , dc_fill_colour          :: RGBi      
      , dc_text_colour          :: RGBi
      , dc_line_spacing_factor  :: Double
      , dc_round_corner_factor  :: Double
      , dc_text_margin          :: TextMargin
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
       { text_margin_x          :: !Double
       , text_margin_y          :: !Double
       }

       -- TODO - this would be preferably as Em or En...

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
-- > font_metrics_table:  empty
-- > font_load_log:       empty
-- > fallback_metrics:    monospace_metrics
-- > font_face:           Courier
-- > font_size:           @supplied_font_size@
-- > stroke_props:        line_width 1, no dash_pattern, cap-butt, join-miter. 
-- > stroke_colour:       black
-- > fill_colour:         light_gray
-- > text_colour:         black
-- > line_spacing_factor: 1.2
-- > round_corner_factor: 0
-- > text_margin:         (2.0 pt, 2.0 pt) 
--
standardContext :: FontSize -> DrawingContext
standardContext sz = 
    DrawingContext { dc_font_metrics_table   = emptyFontTable
                   , dc_font_load_log        = mempty
                   , dc_fallback_metrics     = monospace_metrics
                   , dc_font_face            = wumpus_courier
                   , dc_font_size            = sz
                   , dc_stroke_props         = default_stroke_attr
                   , dc_snap_grid_factors    = (50.0, 50.0)
                   , dc_stroke_colour        = wumpus_black
                   , dc_fill_colour          = wumpus_light_gray
                   , dc_text_colour          = wumpus_black
                   , dc_line_spacing_factor  = default_line_spacing  
                   , dc_round_corner_factor  = default_no_round_corners
                   , dc_text_margin          = default_text_margin
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
metricsContext sz res = 
    let env = standardContext sz 
    in env { dc_font_metrics_table = loaded_font_table res
           , dc_font_load_log      = loader_errors res
           }



-- | 'addFontTables' : @ font_load_result -> DrawinContextUpdate @
--
-- Add the font metrics from the FontLoadResult, if a font with 
-- the same name alreay exists in the 'DrawingContext' it will be 
-- replaced. Error and warning messages in the @font_load_result@ 
-- will be appended to the 'font_load_log'.
--
addFontTables :: FontLoadResult -> DrawingContextF
addFontTables (FontLoadResult table msgs) = 
    (\s i j -> s { dc_font_metrics_table = i `mappend` table
                 , dc_font_load_log      = j `mappend` msgs }) 
      <*> dc_font_metrics_table <*> dc_font_load_log



-- | 'reset_drawing_properties' : @ DrawingContextF @  
--
-- Reset the drawing properties in the 'DrawingContext' to their 
-- default values. This changes the following fields:
--
-- > stroke_props:        line_width 1, no dash_pattern, cap-butt, join-miter. 
-- > stroke_colour:       black
-- > fill_colour:         light_gray
-- > text_colour:         black
-- > line_spacing_factor: 1.2
-- > round_corner_factor: 0
-- > text_margin:         (2.0, 2.0) 
--
reset_drawing_properties :: DrawingContextF 
reset_drawing_properties dcxt = 
    dcxt { dc_stroke_props          = default_stroke_attr
         , dc_stroke_colour         = wumpus_black
         , dc_fill_colour           = wumpus_light_gray
         , dc_text_colour           = wumpus_black
         , dc_line_spacing_factor   = default_line_spacing
         , dc_round_corner_factor   = default_no_round_corners
         , dc_text_margin           = default_text_margin
         }

-- Ideally @reset_drawing_properties@ would be in the UpdateDC 
-- module, but that would mean exporting @default_line_spacing@ 
-- etc.
--



-- | 'reset_drawing_metrics' : @ DrawingContextF @  
--
-- Reset the drawing metrics in the 'DrawingContext' to their 
-- default values. This is a more limited version of
-- 'reset_drawing_properties' and changes the following fields:
--
-- > stroke_props:        line_width 1, no dash_pattern, cap-butt, join-miter. 
-- > line_spacing_factor: 1.2
-- > round_corner_factor: 0
-- > text_margin:         (2.0, 2.0) 
--
reset_drawing_metrics :: DrawingContextF 
reset_drawing_metrics dcxt = 
    dcxt { dc_stroke_props          = default_stroke_attr
         , dc_line_spacing_factor   = default_line_spacing
         , dc_round_corner_factor   = default_no_round_corners
         , dc_text_margin           = default_text_margin
         }



-- Helpers - not exported

default_text_margin :: TextMargin
default_text_margin = TextMargin { text_margin_x = 2.0, text_margin_y = 2.0 }

default_line_spacing :: Double
default_line_spacing = 1.2

default_no_round_corners :: Double
default_no_round_corners = 0

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

-- | 'DrawingCtxM' is equivalent to the to the @MonadReader@ 
-- class, but the environment type is fixed to 'DrawingContext'.
--
-- To avoid name clashes with @mtl@ this scheme is used:
--
-- > queryCtx  = ask
-- > localize  = local
--
-- Note, because the derived operation 'query' (aka @asks@) is
-- expected to be used more often than queryCtx (aka @ask@) it 
-- gets the more convenient name.
--
class (Applicative m, Monad m) => DrawingCtxM (m :: * -> *) where
  queryCtx  :: m DrawingContext
  localize  :: (DrawingContext -> DrawingContext) -> m a -> m a


-- | Project a value out of the DrawingContext.
--
-- This is the analogue to @asks@ in @mtl@.
--
-- > withQuery = asks
-- 
query :: DrawingCtxM m => (DrawingContext -> a) -> m a
query f = queryCtx >>= (return . f)




--------------------------------------------------------------------------------
-- Glyph metrics

-- These are directly on the DrawingContext /for efficiency/.



withFontMetrics :: (FontMetrics -> FontSize -> u) -> DrawingContext -> u
withFontMetrics fn ctx@(DrawingContext { dc_font_face = fface
                                       , dc_font_size = ftsize }) = 
      fn metric_set ftsize
  where 
    ps_name     = ps_font_name fface
    metric_set  = fromMaybe (dc_fallback_metrics ctx) $ 
                    lookupFont ps_name (dc_font_metrics_table ctx)




