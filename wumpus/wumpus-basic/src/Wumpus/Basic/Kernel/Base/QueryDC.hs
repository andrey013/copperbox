{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.QueryDC
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Querying the Drawing Context.
--
-- \*\* WARNING \*\* - parts of this module especially the 
-- mono-space glyph metrics need a re-think and will change or be 
-- dropped.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.QueryDC
  ( 

    normalizeDC
  , normalizeFDC
  , dinterpDC
  , dinterpFDC

  , uconvertFDC

  , pointSize

  , strokeAttr
  , fillAttr
  , borderedAttr
  , textAttr

  , position
  , snapmove

  , roundCornerSize
  , textMargin

  , getLineWidth
  , getFontAttr
  , getFontSize
  , getFontFace
  , getTextColour

  , markHeight
  , markHalfHeight
  , baselineSpacing

  -- * Glyph metrics
  , glyphBoundingBox
  , capHeight
  , descender
  , verticalSpan

  , cwLookupTable


  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.FontMetrics

import Wumpus.Core                              -- package: wumpus-core
import qualified Wumpus.Core.FontSize   as FS
 
import Control.Applicative




normalizeDC :: (DrawingCtxM m, InterpretUnit u) => u -> m Double
normalizeDC u = (\sz -> normalize sz u) <$> pointSize

normalizeFDC :: (DrawingCtxM m, Functor t, InterpretUnit u) 
             => t u -> m (t Double)
normalizeFDC t = (\sz -> fmap (normalize sz) t) <$> pointSize

dinterpDC :: (DrawingCtxM m, InterpretUnit u) => Double -> m u
dinterpDC u = (\sz -> dinterp sz u) <$> pointSize

dinterpFDC :: (DrawingCtxM m, Functor t, InterpretUnit u) => t Double -> m (t u)
dinterpFDC u = (\sz -> fmap (dinterp sz) u) <$> pointSize

uconvertFDC :: (DrawingCtxM m, Functor t, InterpretUnit u, InterpretUnit u1) 
            => t u -> m (t u1)
uconvertFDC t = (\sz -> uconvertF sz t) <$> pointSize


pointSize :: DrawingCtxM m => m FontSize
pointSize = asksDC dc_font_size

strokeAttr :: DrawingCtxM m => m (RGBi, StrokeAttr)
strokeAttr = (,) <$> asksDC dc_stroke_colour <*> asksDC dc_stroke_props

fillAttr :: DrawingCtxM m => m RGBi
fillAttr = asksDC dc_fill_colour


borderedAttr :: DrawingCtxM m => m (RGBi, StrokeAttr, RGBi)
borderedAttr = (,,) <$> asksDC dc_fill_colour 
                    <*> asksDC dc_stroke_props 
                    <*> asksDC dc_stroke_colour



textAttr :: DrawingCtxM m => m (RGBi,FontAttr)
textAttr = 
    (\a b c -> (a, FontAttr b c)) 
      <$> asksDC dc_text_colour <*> asksDC dc_font_size <*> asksDC dc_font_face




-- | Get the Point corresponding the grid coordinates scaled by
-- the snap-grid scaling factors.
--
position :: (DrawingCtxM m, Fractional u) => (Int, Int) -> m (Point2 u)
position (x,y) = post <$> asksDC dc_snap_grid_factors
  where
    post (sx,sy) = P2 (realToFrac $ sx * fromIntegral x) 
                      (realToFrac $ sy * fromIntegral y)




-- | Scale a vector coordinate by the snap-grid scaling factors.
--
-- Absolute units.
--
snapmove :: (DrawingCtxM m, Fractional u) => (Int,Int) -> m (Vec2 u)
snapmove (x,y) = post <$> asksDC dc_snap_grid_factors
  where
    post (sx,sy) = V2 (realToFrac $ sx * fromIntegral x) 
                      (realToFrac $ sy * fromIntegral y)





-- | Size of the round corner factor.
--
roundCornerSize :: (DrawingCtxM m, InterpretUnit u) => m u
roundCornerSize = 
    dinterp <$> asksDC dc_font_size <*> asksDC dc_round_corner_factor





-- | Get the (x,y) margin around text.
--
-- Note - not all text operations in Wumpus are drawn with text 
-- margin. 
-- 
textMargin :: (DrawingCtxM m, InterpretUnit u) => m (u,u)
textMargin = post <$> asksDC dc_font_size <*> asksDC dc_text_margin
  where
    post sz (TextMargin xem yem) = (fn sz xem, fn sz yem)
    fn sz em                     = dinterp sz $ uconvertScalar sz em





getLineWidth :: DrawingCtxM m => m Double
getLineWidth = line_width <$> asksDC dc_stroke_props

getFontAttr :: DrawingCtxM m => m FontAttr
getFontAttr = FontAttr <$> asksDC dc_font_size <*> asksDC dc_font_face


getFontSize     :: DrawingCtxM m => m Int
getFontSize     = asksDC dc_font_size

getFontFace     :: DrawingCtxM m => m FontFace
getFontFace     = asksDC dc_font_face

getTextColour   :: DrawingCtxM m => m RGBi
getTextColour   = asksDC dc_text_colour


-- | The /mark/ height is the height of a lowercase letter in the 
-- Courier font at the current point size.
--
-- Arrowheads, dots etc. should generally be drawn at the mark 
-- height.
-- 
markHeight :: (DrawingCtxM m, InterpretUnit u) => m u
markHeight = post <$> asksDC dc_font_size 
  where
    post sz = dinterp sz (FS.xcharHeight sz)





markHalfHeight :: (Fractional u, DrawingCtxM m, InterpretUnit u) => m u
markHalfHeight = (0.5*) <$> markHeight



-- | Vertical distance between baselines of consecutive text 
-- lines.
--
-- This is a /scaling factor/ hence there is no absolute or
-- relative unit distinction.
-- 
baselineSpacing :: (DrawingCtxM m, Fractional u) => m u
baselineSpacing = post <$> asksDC dc_font_size  <*> asksDC dc_line_spacing_factor
  where
    post sz factor = realToFrac $ factor * fromIntegral sz


--------------------------------------------------------------------------------


glyphQuery :: DrawingCtxM m => (FontMetrics -> FontSize -> a) -> m a
glyphQuery fn = (\ctx -> withFontMetrics fn ctx) <$> askDC



-- | Get the font bounding box - this is the maximum boundary of 
-- the glyphs in the font. The span of the height is expected to 
-- be bigger than the cap_height plus descender depth.
--
glyphBoundingBox :: (DrawingCtxM m, InterpretUnit u) => m (BoundingBox u)
glyphBoundingBox = 
    uconvertF <$> asksDC dc_font_size <*> glyphQuery get_bounding_box



-- | Height of a capital letter.
--
capHeight :: (DrawingCtxM m, InterpretUnit u) => m u
capHeight = dinterp <$> asksDC dc_font_size <*> glyphQuery get_cap_height


-- | Note - descender is expected to be negative.
--
descender :: (DrawingCtxM m, InterpretUnit u) => m u
descender = dinterp <$> asksDC dc_font_size <*> glyphQuery get_descender


-- | This is the distance from cap_height to descender.
--
verticalSpan :: (DrawingCtxM m, InterpretUnit u) => m u
verticalSpan = 
    (\ch dd -> ch - dd) <$> capHeight <*> descender



-- | Note the CharWidthLookup is not parameteric on units.
--
-- /CharWidth/ is always Double representing PostScript points.
-- Client code must convert this value accordingly.
--
cwLookupTable :: DrawingCtxM m => m CharWidthLookup
cwLookupTable = glyphQuery get_cw_table

