{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.GraphicsState
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
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
--   
--------------------------------------------------------------------------------


module Wumpus.Core.GraphicsState 
  (
  -- * Data types  
  
  -- ** Stroke attributes
    StrokeAttr(..)
  , LineCap(..)
  , LineJoin(..)
  , DashPattern(..)

  -- ** Font
  , FontAttr(..)
  , SVGFontStyle(..)

  -- ** Colour
  , PSRgb

  -- ** Current Translation Matrix
  , CTM(..)

  -- * Convert to CTM
  , ToCTM(..)

  -- * Convert to PSColour
  , PSColour(..)

  ) where

import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.Utils

-- Graphics state datatypes

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
data LineCap = CapButt | CapRound | CapSquare
  deriving (Enum,Eq,Show)

-- | Line join - default in output is miter.
data LineJoin = JoinMiter | JoinRound | JoinBevel
  deriving (Enum,Eq,Show)

-- | Dash pattern - either a solid line or a list of on-off pairs
-- together with an /offset/ into the dashes.
data DashPattern = Solid | Dash Int [(Int,Int)]
  deriving (Eq,Show)


-- | Font name and size. Equivalent fonts have different names
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
data FontAttr = FontAttr { 
                    font_name       :: String,        -- for PostScript
                    svg_font_family :: String,        -- for SVG
                    svg_font_style  :: SVGFontStyle,
                    font_size       :: Int 
                  }
  deriving (Eq,Show)

-- | SVG font styles - potentially a style may generate both
-- @font-weight@ and @font-style@ attributes in the SVG output.
data SVGFontStyle = SVG_REGULAR | SVG_BOLD | SVG_ITALIC | SVG_BOLD_ITALIC
                  | SVG_OBLIQUE | SVG_BOLD_OBLIQUE
  deriving (Eq,Show)

type PSRgb = RGB3 Double



-- | PostScript's current transformation matrix.
-- 
-- PostScript and its documentation considers the matrix to be 
-- in this form:
--
-- > | a  b  0 |
-- > | c  d  0 | 
-- > | tx ty 1 |
-- 
-- i.e it considers the homogeneous coordinates of an affine 
-- frame as /rows/ rather than /columns/ (Wumpus uses rows, as 
-- they were the usual representation in the geometry 
-- presentations that inspired it).
-- 
-- Using the component names that we have used in the 
-- description of 'Frame2', the CTM is:
--
-- > | e0x  e0y  0 |
-- > | e1x  e1y  0 | 
-- > | ox   oy   1 |
-- 
-- The CTM is represented in PostScript as an array, using our 
-- names its layout is
--
-- > [ e0x e0y e1x e1y ox oy ] 
--
-- Some examples, the scaling matrix:
--
-- > | sx 0  0 |
-- > | 0  sy 0 |  = [ sx 0 0 sy 0 0 ]
-- > | 0  0  1 |
-- 
-- Translation (displacement) :
--
-- > | 1  0  0 |
-- > | 0  1  0 |  = [ 1 0 0 1 tx ty ]
-- > | tx ty 1 |
-- 
-- Rotation:
-- 
-- > |  cos(a)  sin(a)  0 |
-- > | -sin(a)  cos(a)  0 |  = [ cos(a) sin(a) -sin(a) cos(a) 0 0 ]
-- > |    0       0     1 |

data CTM u = CTM !u !u  !u !u  !u !u
  deriving (Eq,Show)

type instance DUnit (CTM u) = u

--------------------------------------------------------------------------------
-- Conversion to CTM

-- | Convert to the CTM. Wumpus offshores affine transformations 
-- to PostScript as @concat@ commands. So frames and matrices 
-- must support being represented as the CTM.
--
class ToCTM a where 
  toCTM :: u ~ DUnit a => a -> CTM u

instance ToCTM (Frame2 a) where
  toCTM (Frame2 (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)) 
    = CTM e0x e0y  e1x e1y  ox oy
 

instance ToCTM (Matrix3'3 a) where
  toCTM (M3'3 e0x e1x ox  
              e0y e1y oy  
              _   _   _  ) 
    = CTM e0x e0y  e1x e1y  ox oy


--------------------------------------------------------------------------------
-- Conversion to PSColour

-- | Convert to RGB [0,1] for PostScript rendering.
class PSColour a where psColour :: a -> RGB3 Double

instance PSColour (RGB3 Double) where
  psColour (RGB3 r g b) = RGB3 (ramp r) (ramp g) (ramp b)

instance PSColour (HSB3 Double) where
  psColour = psColour . hsb2rgb

instance PSColour (Gray Double) where
  psColour = psColour . gray2rgb




