{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.GraphicsState
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
--
-- Fresh graphics state. 
-- 
--   
--------------------------------------------------------------------------------


module Wumpus.Fresh.GraphicsState 
  (
  -- * Data types  
    GraphicsState(..)
  
  -- ** Stroke attributes
  , StrokeAttr(..)
  , LineCap(..)
  , LineJoin(..)
  , DashPattern(..)

  -- ** Font
  , FontAttr(..)
  , FontFace(..)
  , SVGFontStyle(..)

  -- ** Default graphic state
  , zeroGS

  ) where

import Wumpus.Fresh.Colour

-- Graphics state datatypes

data GraphicsState = GraphicsState
      { gs_draw_colour  :: RGB255
      , gs_font_size    :: Int
      , gs_font_face    :: FontFace
      , gs_line_width   :: Double
      , gs_miter_limit  :: Double
      , gs_line_cap     :: LineCap
      , gs_line_join    :: LineJoin
      , gs_dash_pattern :: DashPattern 
      }
  deriving (Eq,Show)


-- | Stroke attributes are an algebriac type rather than a 
-- record type. This is for convenience when attributing paths -
-- paths can be attibuted with just the differences from the 
-- default settings, rather than all the settings whether or not
-- they are important.
--
data StrokeAttr = LineWidth   Double
                | MiterLimit  Double
                | LineCap     LineCap
                | LineJoin    LineJoin
                | DashPattern DashPattern 
  deriving (Eq,Show)


-- | Line cap - default in output is butt.
--
data LineCap = CapButt | CapRound | CapSquare
  deriving (Enum,Eq,Show)

-- | Line join - default in output is miter.
--
data LineJoin = JoinMiter | JoinRound | JoinBevel
  deriving (Enum,Eq,Show)

-- | Dash pattern - either a solid line or a list of on-off pairs
-- together with an /offset/ into the dashes.
--
data DashPattern = Solid | Dash Int [(Int,Int)]
  deriving (Eq,Show)


-- | Font face and size. Equivalent fonts have different names
-- in PostScript and SVG. A PostScript font name includes the 
-- font style (e.g. @Times-BoldItalic@) whereas an SVG font has 
-- a name (the @font-family@ attribute) and a style.
--
-- For PostScript, the following fonts are expected to exist on 
-- most platforms:
--
-- > Times-Roman  Times-Italic  Times-Bold  Times-BoldOtalic
-- > Helvetica  Helvetica-Oblique  Helvetica-Bold  Helvetica-Bold-Oblique
-- > Courier  Courier-Oblique  Courier-Bold  Courier-Bold-Oblique
-- > Symbol
--
-- See the PostScript Language Reference Manual.
--
data FontAttr = FontAttr 
      { font_size  :: Int 
      , font_face  :: FontFace
      }
  deriving (Eq,Ord,Show)

-- | 'FontFace' : @ postscript_name * svg_font_family * svg_font_style @
--
data FontFace = FontFace
      { font_name       :: String        -- for PostScript
      , svg_font_family :: String        -- for SVG
      , svg_font_style  :: SVGFontStyle
      }
  deriving (Eq,Ord,Show)



-- | SVG font styles - potentially a style may generate both
-- @font-weight@ and @font-style@ attributes in the SVG output.
--
data SVGFontStyle = SVG_REGULAR | SVG_BOLD | SVG_ITALIC | SVG_BOLD_ITALIC
                  | SVG_OBLIQUE | SVG_BOLD_OBLIQUE
  deriving (Eq,Ord,Show)


-- | The initial graphics state

zeroGS ::  GraphicsState 
zeroGS = GraphicsState { gs_draw_colour  = black
                       , gs_font_size    = (-1)
                       , gs_font_face    = unmatchable_face
                       , gs_line_width   = 1
                       , gs_miter_limit  = 1
                       , gs_line_cap     = CapButt
                       , gs_line_join    = JoinMiter
                       , gs_dash_pattern = Solid
                       }
  where
    unmatchable_face = FontFace "DONT_MATCH" "" SVG_BOLD_OBLIQUE
