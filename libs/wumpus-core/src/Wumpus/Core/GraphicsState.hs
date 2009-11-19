{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.GraphicsState
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data types modelling the Graphics state
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


data StrokeAttr = LineWidth   Double
                | MiterLimit  Double
                | LineCap     LineCap
                | LineJoin    LineJoin
                | DashPattern DashPattern 
  deriving (Eq,Show)

data LineCap = CapButt | CapRound | CapSquare
  deriving (Enum,Eq,Show)

data LineJoin = JoinMiter | JoinRound | JoinBevel
  deriving (Enum,Eq,Show)

data DashPattern = Solid | Dash Int [Int]
  deriving (Eq,Show)


-- PostScript (or at least GhostScript) seems to require both
-- attributes (name & size) are set at the same time.

data FontAttr = FontAttr { 
                    font_name       :: String,        -- for PostScript
                    svg_font_family :: String,        -- for SVG
                    svg_font_style  :: SVGFontStyle,
                    font_size       :: Int 
                  }
  deriving (Eq,Show)

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

class PSColour a where psColour :: a -> RGB3 Double

instance PSColour (RGB3 Double) where
  psColour (RGB3 r g b) = RGB3 (ramp r) (ramp g) (ramp b)

instance PSColour (HSB3 Double) where
  psColour = psColour . hsb2rgb

instance PSColour (Gray Double) where
  psColour = psColour . gray2rgb




