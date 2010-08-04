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

  ) where


import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative

data DrawingAttr = DrawingAttr 
      { line_width         :: Double
      , font_props         :: FontAttr
      , stroke_colour      :: DRGB
      , fill_colour        :: DRGB
      }
  deriving (Eq,Show)

standardAttr :: FontSize -> DrawingAttr
standardAttr sz = DrawingAttr { line_width         = 1.0
                              , font_props         = courier sz
                              , stroke_colour      = black
                              , fill_colour        = gold  }

 
strokeAttr :: DrawingAttr -> (DRGB, StrokeAttr)
strokeAttr = liftA2 (,) stroke_colour (LineWidth . line_width)

fillAttr :: DrawingAttr -> DRGB
fillAttr = fill_colour

textAttr :: DrawingAttr -> (DRGB,FontAttr)
textAttr = liftA2 (,) stroke_colour font_props

-- | A Mark is consider to be the height of a lowercase letter
-- in the current font.
--
markHeight :: Fractional u => DrawingAttr -> u
markHeight = xcharHeight . font_size . font_props


-- | textDimensions : text -> DrawingAttr -> (width,height)
--
textDimensions :: Fractional u => String -> DrawingAttr -> (u,u)
textDimensions str attr = (w,h)
  where
    sz = font_size $ font_props attr
    w  = 0.5 * textWidth sz (length str) 
    h  = 0.5 * textHeight sz
