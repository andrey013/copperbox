{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.GraphicsState
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Data types for stroke and label attributes and type classes 
-- for conversion to PostScript\'s colour and matrix 
-- representations. 
-- 
-- Wumpus represents pictures as trees - a leaf represents a 
-- path or text label. All attributes of a path or text label 
-- (colour, stroke width, font, ...) are stored in the leaf. So
-- a picture is a leaf labelled tree.
-- 
-- By contrast, PostScript maintains a /graphics state/. A 
-- PostScript program is free to modify the graphics state 
-- anywhere in the program. Stroke width is a general property  
-- shared by all elements (initially it has the default value 1).
-- Only stroked paths actually regard stroke width, fonts and 
-- filled and clipping paths ignore it. PostScript allows more 
-- control over the graphics state by allowing the current state
-- to be saved and restored with the @gsave@ and @grestore@. 
-- This is useful for modularity but is a comparatively expensive
-- procedure.
--
-- When Wumpus renders Pictures as PostScript it maintains a 
-- limited graphics state with just current colour and current 
-- font. This is so Wumpus can avoid repeating @setrgbcolor@ and
-- @findfont@ operations in the generated PostScript if 
-- subsequent elements share the same values.
-- 
--   
--------------------------------------------------------------------------------


module Wumpus.Core.GraphicsState 
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

  -- * Initial graphic state
  , zeroGS
  , defaultSA

  ) where

import Wumpus.Core.Colour

-- Graphics state datatypes

-- | Graphics state used by the rendering monads.
--
-- This type is hidden by the top-level module @Wumpus.Core@.
--
data GraphicsState = GraphicsState
      { gs_draw_colour  :: RGBi
      , gs_font_size    :: Int
      , gs_font_face    :: FontFace
      , gs_stroke_attr  :: StrokeAttr 
      }
  deriving (Eq,Show)

-- | Stroke attributes for drawing paths.
--
data StrokeAttr = StrokeAttr
      { line_width      :: Double
      , miter_limit     :: Double
      , line_cap        :: LineCap
      , line_join       :: LineJoin
      , dash_pattern    :: DashPattern
      }
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
                       , gs_stroke_attr  = defaultSA
                       }
  where
    unmatchable_face = FontFace "DONT_MATCH" "" SVG_BOLD_OBLIQUE


-- | Default stroke attributes.
--
defaultSA :: StrokeAttr
defaultSA = StrokeAttr { line_width      = 1
                       , miter_limit     = 1
                       , line_cap        = CapButt
                       , line_join       = JoinMiter
                       , dash_pattern    = Solid
                       }
