{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.DrawingAttr
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Drawing attributes
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.DrawingAttr
  ( 

  -- * Drawing attributes
    DrawingAttr(..)

  , standardAttr
  , strokeAttr   
  , fillAttr
  , textAttr
  , markHeight

  , textDimensions

  -- * Line widths
  , thick
  , ultrathick
  , thin

  -- * Font properties
  , fontsize
  , fontface

  ) where


import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Utils.Combinators

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative

data DrawingAttr = DrawingAttr 
      { stroke_props       :: StrokeAttr
      , font_props         :: FontAttr
      , stroke_colour      :: RGBi
      , fill_colour        :: RGBi
      }
  deriving (Eq,Show)

standardAttr :: FontSize -> DrawingAttr
standardAttr sz = DrawingAttr { stroke_props       = defaultSA
                              , font_props         = FontAttr sz courier
                              , stroke_colour      = black
                              , fill_colour        = gold  }

 
strokeAttr :: DrawingAttr -> (RGBi, StrokeAttr)
strokeAttr = liftA2 (,) stroke_colour stroke_props

fillAttr :: DrawingAttr -> RGBi
fillAttr = fill_colour

textAttr :: DrawingAttr -> (RGBi,FontAttr)
textAttr = liftA2 (,) stroke_colour font_props

-- | A Mark is consider to be the height of a lowercase letter
-- in the current font.
--
markHeight :: FromPtSize u => DrawingAttr -> u
markHeight = fromPtSize . xcharHeight . font_size . font_props


-- | textDimensions : text -> DrawingAttr -> (width,height)
--
textDimensions :: FromPtSize u => String -> DrawingAttr -> (u,u)
textDimensions str attr = (w,h)
  where
    sz = font_size  $ font_props attr
    w  = fromPtSize $ textWidth  sz (1 + length str) 
    h  = fromPtSize $ textHeight sz



--------------------------------------------------------------------------------
-- line widths

-- Note - some care might be needed if we ever define other unit 
-- types...

-- std_line_width      :: Double
-- std_line_width      = 1.0

thick_line          :: Double
thick_line          = 2.0

ultra_thick_line    :: Double
ultra_thick_line    = 4.0

thin_line           :: Double
thin_line           = 0.5

setLineWidth       :: Double -> DrawingAttr -> DrawingAttr
setLineWidth d      = star (\s i -> s { stroke_props = upd i} ) stroke_props
  where
   upd attrs        = attrs { line_width = d }


thick               :: DrawingAttr -> DrawingAttr
thick               = setLineWidth thick_line

ultrathick          :: DrawingAttr -> DrawingAttr
ultrathick          = setLineWidth ultra_thick_line

thin                :: DrawingAttr -> DrawingAttr
thin                = setLineWidth thin_line


fontface            :: FontFace -> DrawingAttr -> DrawingAttr
fontface ff         = star (\s i -> s { font_props = upd i }) font_props
  where
    upd (FontAttr sz _) = FontAttr sz ff

fontsize            :: Int -> DrawingAttr -> DrawingAttr
fontsize sz         = star (\s i -> s { font_props = upd i }) font_props
  where
    upd (FontAttr _ ff) = FontAttr sz ff