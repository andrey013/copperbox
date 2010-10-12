{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.GraphicProps
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Data types for stroke and label styles corresponding to the
-- styles provided by PostScript / SVG.
--
-- Data types for annotating Primitives (@PathProps@ etc.) with 
-- their drawing style. Wumpus represents pictures as trees and
-- decorates all elements (paths, text-labels) with their drawing
-- style. This is boardly similar to how SVG handles attributes. 
-- For PostScript output, Wumpus renders attribute changes as 
-- graphics state updates. 
--   
--------------------------------------------------------------------------------


module Wumpus.Core.GraphicProps
  (
  
  -- * Stroke attributes
    StrokeAttr(..)
  , LineCap(..)
  , LineJoin(..)
  , DashPattern(..)

  -- * Font
  , FontAttr(..)
  , FontFace(..)
  , SVGFontStyle(..)


  -- * Drawing styles for Primitives
  , PathProps(..)
  , LabelProps(..)
  , EllipseProps(..)

  -- * Defaults
  , default_stroke_attr
  , wumpus_default_font

  ) where

import Wumpus.Core.Colour
import Wumpus.Core.Text.Encoder
import Wumpus.Core.Text.Latin1
import Wumpus.Core.Utils.FormatCombinators



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
      { font_size       :: Int 
      , font_face       :: FontFace
      }
  deriving (Eq,Ord,Show)

-- | 'FontFace' : @ postscript_name * svg_font_family * svg_font_style @
--
data FontFace = FontFace
      { font_name       :: String        -- for PostScript
      , svg_font_family :: String        -- for SVG
      , svg_font_style  :: SVGFontStyle
      , font_enc_name   :: FontEncoderName
      }
  deriving (Eq,Ord,Show)



-- | SVG font styles - potentially a style may generate both
-- @font-weight@ and @font-style@ attributes in the SVG output.
--
data SVGFontStyle = SVG_REGULAR | SVG_BOLD | SVG_ITALIC | SVG_BOLD_ITALIC
                  | SVG_OBLIQUE | SVG_BOLD_OBLIQUE
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------

-- | Note when drawn /filled/ and drawn /stroked/ the same
-- polygon will have (slightly) different size:
--
-- * A filled shape fills /within/ the boundary of the shape
--
-- * A stroked shape draws a pen line around the boundary
--   of the shape. The actual size depends on the thickness
--   of the line (stroke width).
--
data PathProps = CFill RGBi 
               | CStroke StrokeAttr RGBi
               | OStroke StrokeAttr RGBi
               -- Note - first colour fill, second colur stroke.
               | CFillStroke RGBi StrokeAttr RGBi
  deriving (Eq,Show)


-- | Font rendering properties for a PrimLabel.
--
data LabelProps = LabelProps 
      { label_colour    :: RGBi
      , label_font      :: FontAttr
      }
  deriving (Eq,Ord,Show)



-- | Ellipses and circles are always closed.
--
data EllipseProps = EFill RGBi
                  | EStroke StrokeAttr RGBi 
                  -- Note - first colour fill, second colour stroke.
                  | EFillStroke RGBi StrokeAttr RGBi 
  deriving (Eq,Show)




--------------------------------------------------------------------------------

instance Format PathProps where
  format (CFill rgb)          = format rgb <+> text "Fill"
  format (CStroke _ rgb)      = format rgb <+> text "Closed-stroke"
  format (OStroke _ rgb)      = format rgb <+> text "Open-stroke"
  format (CFillStroke f _ s)  = format f <+> text "Fill" <> char '/'
                                         <+> format s <+> text "Stroke"   



instance Format LabelProps where
  format (LabelProps rgb attr) = format rgb 
                             <+> text (font_name $ font_face attr)

instance Format EllipseProps where
  format (EFill rgb)          = format rgb <+> text "Fill"
  format (EStroke _ rgb)      = format rgb <+> text "Stroke"
  format (EFillStroke f _ s)  = format f <+> text "Fill" <> char '/'
                            <+> format s <+> text "Stroke"   

--------------------------------------------------------------------------------
-- Defaults

-- | Default stroke attributes.
--
default_stroke_attr :: StrokeAttr
default_stroke_attr = StrokeAttr { line_width      = 1
                                 , miter_limit     = 1
                                 , line_cap        = CapButt
                                 , line_join       = JoinMiter
                                 , dash_pattern    = Solid
                                 }

-- | Constant for the default font, which is @Courier@ (aliased 
-- to @Courier New@ for SVG) at 14 point.
--
--
wumpus_default_font :: FontAttr
wumpus_default_font = FontAttr 14 face 
  where
    face = FontFace { font_name         = "Courier"
                    , svg_font_family   = "Courier New"
                    , svg_font_style    = SVG_REGULAR
                    , font_enc_name     = latin1_font_encoder
                    }
