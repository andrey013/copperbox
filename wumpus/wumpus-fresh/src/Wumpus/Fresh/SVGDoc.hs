{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.SVGDoc
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh SVG.
--
--------------------------------------------------------------------------------

module Wumpus.Fresh.SVGDoc
  (
    escapeSpecial
  
  , elem_path
  , elem_text
  , elem_tspan
  , elem_ellipse
  , elem_circle

  , attr_x
  , attr_y
  , attr_r
  , attr_rx
  , attr_ry
  , attr_cx
  , attr_cy

  , path_m
  , path_l
  , path_c

  , val_rgb

  , attr_font_family
  , attr_font_size
  , attr_font_weight
  , attr_font_style

  , attr_fill
  , attr_fill_none
  , attr_stroke
  , attr_stroke_none
  , attr_stroke_width

  , attr_transform
  , val_matrix

  ) where

import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.FreshIR
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.TextEncoder
import Wumpus.Fresh.Utils


escapeSpecial :: CharCode -> String
escapeSpecial i = "&#" ++ show i ++ ";"



-- Note - it is easier put particular attrs at the end (esp. d 
-- for paths) if attrs are treated as a Doc. 


svgElem :: String -> Doc -> Doc
svgElem name attrs = angles (text name <+> attrs <+> char '/')

svgElemB :: String -> Doc -> [Doc] -> Doc
svgElemB name attrs elems = vcat [ open, indentLines 2 elems, close ]
  where
    open  = angles (text name <+> attrs)
    close = angles (char '/' <> text name)

-- 1 line version of svgElemB
--
svgElemB1 :: String -> Doc -> Doc -> Doc
svgElemB1 name attrs elems = open <> elems <> close
  where
    open  = angles (text name <+> attrs)
    close = angles (char '/' <> text name)


svgAttr :: String -> Doc -> Doc
svgAttr name val = text name <> char '=' <> dquotes val
 


--------------------------------------------------------------------------------
-- | @ \<path ... d=... /\> @
--
elem_path :: Doc -> Doc -> Doc
elem_path attrs path = svgElem "path" (attrs <+> svgAttr "d" path)


-- | @ \<text ... >...\</text\> @
--
elem_text :: Doc -> Doc -> Doc
elem_text attrs body1 = svgElemB "text" attrs [body1]


-- | @ \<tspan ... >...\</tspan\> @
--
elem_tspan :: Doc -> Doc -> Doc
elem_tspan attrs body1 = svgElemB1 "text" attrs body1

-- @ \<circle ... /\>
--
elem_circle :: Doc -> Doc
elem_circle attrs = svgElem "circle" attrs

-- @ \<ellipse ... /\>
--
elem_ellipse :: Doc -> Doc
elem_ellipse attrs = svgElem "ellipse" attrs


-- | @ x=\"...\" @
--
attr_x :: PSUnit u => u -> Doc
attr_x = svgAttr "x" . dtruncFmt

-- | @ y=\"...\" @
--
attr_y :: PSUnit u => u -> Doc
attr_y = svgAttr "y" . dtruncFmt

-- | @ r=\"...\" @
--
attr_r :: PSUnit u => u -> Doc
attr_r = svgAttr "r" . dtruncFmt


-- | @ rx=\"...\" @
--
attr_rx :: PSUnit u => u -> Doc
attr_rx = svgAttr "rx" . dtruncFmt

-- | @ ry=\"...\" @
--
attr_ry :: PSUnit u => u -> Doc
attr_ry = svgAttr "ry" . dtruncFmt

-- | @ cx=\"...\" @
--
attr_cx :: PSUnit u => u -> Doc
attr_cx = svgAttr "cx" . dtruncFmt

-- | @ cy=\"...\" @
--
attr_cy :: PSUnit u => u -> Doc
attr_cy = svgAttr "cy" . dtruncFmt




--------------------------------------------------------------------------------
-- Path Segments, encoded as string values.


-- | @ M ... ... @
--
-- c.f. PostScript's @moveto@.
--
path_m :: PSUnit u => Point2 u -> Doc
path_m (P2 x y) = char 'M' <+> dtruncFmt x <+> dtruncFmt y

-- | @ L ... ... @
--
-- c.f. PostScript's @lineto@.
--
path_l :: PSUnit u => Point2 u -> Doc
path_l (P2 x y) = char 'L' <+> dtruncFmt x <+> dtruncFmt y

-- | @ C ... ... ... ... ... ... @
-- 
-- c.f. PostScript's @curveto@.
--
path_c :: PSUnit u => Point2 u -> Point2 u -> Point2 u -> Doc
path_c (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) =
    char 'C' <+> dtruncFmt x1 <+> dtruncFmt y1
             <+> dtruncFmt x2 <+> dtruncFmt y2
             <+> dtruncFmt x3 <+> dtruncFmt y3


val_rgb :: RGB255 -> Doc
val_rgb (RGB255 r g b) = 
    text "rgb" <> tupled [integral r, integral g, integral b]


-- | @ font-family=\"...\" @
--
attr_font_family :: String -> Doc
attr_font_family = svgAttr "font-family" . text

-- | @ font-size=\"...\" @
--
attr_font_size :: Int -> Doc
attr_font_size = svgAttr "font-size" . int

-- | @ font-weight=\"...\" @
--
attr_font_weight :: String -> Doc
attr_font_weight = svgAttr "font-weight" . text

-- | @ font-style=\"...\" @
--
attr_font_style :: String -> Doc
attr_font_style = svgAttr "font-style" . text


-- | @ fill=\"rgb(..., ..., ...)\" @
--
attr_fill :: RGB255 -> Doc
attr_fill = svgAttr "fill" . val_rgb 

-- | @ fill=\"none\" @
--
attr_fill_none :: Doc
attr_fill_none = svgAttr "fill" (text "none")

-- | @ stroke=\"rgb(..., ..., ...)\" @
--
attr_stroke :: RGB255 -> Doc
attr_stroke = svgAttr "stroke" . val_rgb

-- | @ stroke=\"none\" @
--
attr_stroke_none :: Doc
attr_stroke_none = svgAttr "stroke" (text "none")

-- | @ stroke-width=\"...\" @
--
attr_stroke_width :: PSUnit u => u -> Doc
attr_stroke_width = svgAttr "stroke-width" . dtruncFmt

-- | @ transform="..." @
--
attr_transform :: Doc -> Doc
attr_transform = svgAttr "transform"

-- | @ matrix(..., ..., ..., ..., ..., ...) @
--
val_matrix :: PSUnit u => Matrix3'3 u -> Doc
val_matrix mtrx = text "matrix" <> tupled (map dtruncFmt [a,b,c,d,e,f])
  where
    (a,b,c,d,e,f) = deconsMatrix mtrx