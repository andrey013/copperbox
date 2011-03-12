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

    DCQuery
  , stroke_attr
  , fill_attr
  , bordered_attr
  , point_size
  , text_attr

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


type DCQuery a = DrawingContext -> a

stroke_attr :: DCQuery (RGBi, StrokeAttr)
stroke_attr = (,) <$> dc_stroke_colour <*> dc_stroke_props

fill_attr :: DCQuery RGBi
fill_attr = dc_fill_colour


bordered_attr :: DCQuery (RGBi, StrokeAttr, RGBi)
bordered_attr = (,,) <$> dc_fill_colour <*> dc_stroke_props 
                                        <*> dc_stroke_colour

point_size :: DCQuery FontSize
point_size = dc_font_size


text_attr :: DCQuery (RGBi,FontAttr)
text_attr = (\a b c -> (a, FontAttr b c)) 
              <$> dc_text_colour <*> dc_font_size <*> dc_font_face




-- | Get the Point corresponding the grid coordinates scaled by
-- the snap-grid scaling factors.
--
position :: Fractional u => (Int, Int) -> DCQuery (Point2 u)
position (x,y) = post <$> dc_snap_grid_factors
  where
    post (sx,sy) = P2 (realToFrac $ sx * fromIntegral x) 
                      (realToFrac $ sy * fromIntegral y)




-- | Scale a vector coordinate by the snap-grid scaling factors.
--
-- Absolute units.
--
snapmove :: Fractional u => (Int,Int) -> DCQuery (Vec2 u)
snapmove (x,y) = post <$> dc_snap_grid_factors
  where
    post (sx,sy) = V2 (realToFrac $ sx * fromIntegral x) 
                      (realToFrac $ sy * fromIntegral y)





-- | Size of the round corner factor.
--
roundCornerSize :: InterpretUnit u => DCQuery u
roundCornerSize = dinterp <$> dc_font_size <*> dc_round_corner_factor





-- | Get the (x,y) margin around text.
--
-- Note - not all text operations in Wumpus are drawn with text 
-- margin. 
-- 
textMargin :: InterpretUnit u => DCQuery (u,u)
textMargin = post <$> dc_font_size <*> dc_text_margin
  where
    post sz (TextMargin xem yem) = (fn sz xem, fn sz yem)
    fn sz em                     = dinterp sz $ uconvertScalar sz em





getLineWidth :: DCQuery Double
getLineWidth = line_width <$> dc_stroke_props

getFontAttr :: DCQuery FontAttr
getFontAttr = FontAttr <$> dc_font_size <*> dc_font_face


-- These are not especially convenient now the drawing context 
-- has changed...
getFontSize :: DCQuery Int
getFontSize = dc_font_size

getFontFace :: DCQuery FontFace
getFontFace = dc_font_face

getTextColour :: DCQuery RGBi
getTextColour = dc_text_colour


-- | The /mark/ height is the height of a lowercase letter in the 
-- Courier font at the current point size.
--
-- Arrowheads, dots etc. should generally be drawn at the mark 
-- height.
-- 
markHeight :: InterpretUnit u => DCQuery u
markHeight = post <$> dc_font_size 
  where
    post sz = dinterp sz (FS.xcharHeight sz)





markHalfHeight :: (Fractional u, InterpretUnit u) => DCQuery u
markHalfHeight = (0.5*) <$> markHeight



-- | Vertical distance between baselines of consecutive text 
-- lines.
--
-- This is a /scaling factor/ hence there is no absolute or
-- relative unit distinction.
-- 
baselineSpacing :: Fractional u => DCQuery u
baselineSpacing = post <$> dc_font_size  <*> dc_line_spacing_factor
  where
    post sz factor = realToFrac $ factor * fromIntegral sz


--------------------------------------------------------------------------------


glyphQuery :: (FontMetrics -> FontSize -> a) -> DCQuery a
glyphQuery fn = (\ctx -> withFontMetrics fn ctx)



-- | Get the font bounding box - this is the maximum boundary of 
-- the glyphs in the font. The span of the height is expected to 
-- be bigger than the cap_height plus descender depth.
--
glyphBoundingBox :: InterpretUnit u => DCQuery (BoundingBox u)
glyphBoundingBox = uconvertExt <$> dc_font_size <*> glyphQuery get_bounding_box



-- | Height of a capital letter.
--
capHeight :: InterpretUnit u => DCQuery u
capHeight = dinterp <$> dc_font_size <*> glyphQuery get_cap_height


-- | Note - descender is expected to be negative.
--
descender :: InterpretUnit u => DCQuery u
descender = dinterp <$> dc_font_size <*> glyphQuery get_descender


-- | This is the distance from cap_height to descender.
--
verticalSpan :: InterpretUnit u => DCQuery u
verticalSpan = 
    (\ch dd -> ch - dd) <$> capHeight <*> descender



-- | Note the CharWidthLookup is not parameteric on units.
--
-- /CharWidth/ is always Double representing PostScript points.
-- Client code must convert this value accordingly.
--
cwLookupTable :: DCQuery CharWidthLookup
cwLookupTable = glyphQuery get_cw_table

