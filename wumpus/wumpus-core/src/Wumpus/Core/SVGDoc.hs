{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.SVGDoc
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- SVG Doc combinators.
--
--------------------------------------------------------------------------------

module Wumpus.Core.SVGDoc
  (
    escapeSpecial
  , svgAttr
  , xml_version
  , doctype
  , elem_svg
  , elem_svg_defs
  , elem_g
  , elem_g_no_attrs

  , elem_a_xlink  
  , elem_clipPath
  , elem_path
  , elem_path_no_attrs
  , elem_text
  , elem_tspan
  , elem_ellipse
  , elem_circle

  , attr_id
  , attr_x
  , attr_xs
  , attr_y
  , attr_ys
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
  , attr_stroke_miterlimit
  , attr_stroke_linecap
  , attr_stroke_linejoin

  , attr_stroke_dasharray
  , attr_stroke_dasharray_none
  , attr_stroke_dashoffset

  , attr_clip_path
  , attr_transform
  , val_matrix
  , val_translate

  ) where

import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicProps
import Wumpus.Core.PictureInternal
import Wumpus.Core.PtSize
import Wumpus.Core.Utils.FormatCombinators


escapeSpecial :: Int -> Doc
escapeSpecial i = text "&#" <> int i <> char ';'


-- Note - it is easier put particular attrs at the end (esp. d 
-- for paths) if attrs are treated as a Doc. 


svgElem :: String -> Doc -> Doc
svgElem name attrs = angles (text name <+> attrs <+> char '/')

svgElemB :: String -> Doc -> Doc -> Doc
svgElemB name attrs body = vcat [ open, indent 2 body, close ]
  where
    open  = angles (text name <+> attrs)
    close = angles (char '/' <> text name)

svgElemB_no_attrs :: String -> Doc -> Doc
svgElemB_no_attrs name body = vcat [ open, indent 2 body, close ]
  where
    open  = angles (text name)
    close = angles (char '/' <> text name)

-- 1 line version of svgElemB
--
svgElemB1 :: String -> Doc -> Doc -> Doc
svgElemB1 name attrs body = open <> body <> close
  where
    open  = angles (text name <+> attrs)
    close = angles (char '/' <> text name)


svgAttr :: String -> Doc -> Doc
svgAttr name val = text name <> char '=' <> dquotes val
 
dquoteText :: String -> Doc
dquoteText = dquotes . text 

--------------------------------------------------------------------------------
xml_version :: Doc
xml_version = text "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"

doctype :: Doc 
doctype = angles (    text "!DOCTYPE svg PUBLIC" 
                  <+> dquoteText "-//W3C//DTD SVG 1.1//EN"
                  <+> dquoteText svg_url )
  where
    svg_url = "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"


elem_svg :: Doc -> Doc 
elem_svg body = svgElemB "svg" svg_top_attrs body

elem_svg_defs :: String -> Doc -> Doc 
elem_svg_defs ss body = 
    svgElemB "svg" svg_top_attrs (elem_defs ss `vconcat` body)


svg_top_attrs :: Doc
svg_top_attrs = svgns <+> svgvn <+> xlink
  where 
    svgns = svgAttr "xmlns"       (text "http://www.w3.org/2000/svg")
    svgvn = svgAttr "version"     (text "1.1")
    xlink = svgAttr "xmlns:xlink" (text "http://www.w3.org/1999/xlink")

-- | @ \<g ...\> ... \</g\> @ 
--
elem_g :: Doc -> Doc -> Doc
elem_g attrs body = svgElemB "g" attrs body

-- | @ \<g\> ... \<g/\> @ 
--
elem_g_no_attrs :: Doc -> Doc
elem_g_no_attrs body = svgElemB_no_attrs "g" body

elem_defs :: String -> Doc
elem_defs ss = svgElemB_no_attrs "defs" $ vcat $ map text $ lines ss


-- | @ \<clipPath ...\> ... \</clipPath\> @ 
--
elem_clipPath :: Doc -> Doc -> Doc
elem_clipPath attrs body = svgElemB "clipPath" attrs body


elem_a_xlink :: String -> Doc -> Doc
elem_a_xlink href body = svgElemB "a" attrs body
  where
    attrs = svgAttr "xlink:href" (text href)



-- | @ \<path ... d=... /\> @
--
elem_path :: Doc -> Doc -> Doc
elem_path attrs path = svgElem "path" (attrs <+> svgAttr "d" path)


-- | @ \<path d=... /\> @
--
elem_path_no_attrs :: Doc -> Doc
elem_path_no_attrs path = svgElem "path" (svgAttr "d" path)


-- | @ \<text ... >...\</text\> @
--
elem_text :: Doc -> Doc -> Doc
elem_text attrs body = svgElemB "text" attrs body


-- | @ \<tspan ... >...\</tspan\> @
--
elem_tspan :: Doc -> Doc -> Doc
elem_tspan attrs body1 = svgElemB1 "tspan" attrs body1

-- @ \<circle ... /\>
--
elem_circle :: Doc -> Doc
elem_circle attrs = svgElem "circle" attrs

-- @ \<ellipse ... /\>
--
elem_ellipse :: Doc -> Doc
elem_ellipse attrs = svgElem "ellipse" attrs

-- | @ id=\"...\" @
--
attr_id :: String -> Doc
attr_id = svgAttr "id" . text


-- | @ x=\"...\" @
--
attr_x :: PtSize u => u -> Doc
attr_x = svgAttr "x" . psptFmt


-- | @ x=\"... ... ...\" @
--
-- /List/ version of attr_x
-- 
attr_xs :: PtSize u => [u] -> Doc
attr_xs = svgAttr "x" . hsep . map psptFmt


-- | @ y=\"...\" @
--
attr_y :: PtSize u => u -> Doc
attr_y = svgAttr "y" . psptFmt


-- | @ y=\"... ... ...\" @
--
-- /List/ version of attr_y
-- 
attr_ys :: PtSize u => [u] -> Doc
attr_ys = svgAttr "y" . hsep . map psptFmt


-- | @ r=\"...\" @
--
attr_r :: PtSize u => u -> Doc
attr_r = svgAttr "r" . psptFmt


-- | @ rx=\"...\" @
--
attr_rx :: PtSize u => u -> Doc
attr_rx = svgAttr "rx" . psptFmt

-- | @ ry=\"...\" @
--
attr_ry :: PtSize u => u -> Doc
attr_ry = svgAttr "ry" . psptFmt

-- | @ cx=\"...\" @
--
attr_cx :: PtSize u => u -> Doc
attr_cx = svgAttr "cx" . psptFmt

-- | @ cy=\"...\" @
--
attr_cy :: PtSize u => u -> Doc
attr_cy = svgAttr "cy" . psptFmt




--------------------------------------------------------------------------------
-- Path Segments, encoded as string values.


-- | @ M ... ... @
--
-- c.f. PostScript's @moveto@.
--
path_m :: PtSize u => Point2 u -> Doc
path_m (P2 x y) = char 'M' <+> psptFmt x <+> psptFmt y

-- | @ L ... ... @
--
-- c.f. PostScript's @lineto@.
--
path_l :: PtSize u => Point2 u -> Doc
path_l (P2 x y) = char 'L' <+> psptFmt x <+> psptFmt y

-- | @ C ... ... ... ... ... ... @
-- 
-- c.f. PostScript's @curveto@.
--
path_c :: PtSize u => Point2 u -> Point2 u -> Point2 u -> Doc
path_c (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) =
    char 'C' <+> psptFmt x1 <+> psptFmt y1
             <+> psptFmt x2 <+> psptFmt y2
             <+> psptFmt x3 <+> psptFmt y3


val_rgb :: RGBi -> Doc
val_rgb (RGBi r g b) = 
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
attr_fill :: RGBi -> Doc
attr_fill = svgAttr "fill" . val_rgb 

-- | @ fill=\"none\" @
--
attr_fill_none :: Doc
attr_fill_none = svgAttr "fill" (text "none")

-- | @ stroke=\"rgb(..., ..., ...)\" @
--
attr_stroke :: RGBi -> Doc
attr_stroke = svgAttr "stroke" . val_rgb

-- | @ stroke=\"none\" @
--
attr_stroke_none :: Doc
attr_stroke_none = svgAttr "stroke" (text "none")

-- | @ stroke-width=\"...\" @
--
attr_stroke_width :: PtSize u => u -> Doc
attr_stroke_width = svgAttr "stroke-width" . psptFmt


-- | @ stroke-miterlimit=\"...\" @
--
attr_stroke_miterlimit :: PtSize u => u -> Doc
attr_stroke_miterlimit = svgAttr "stroke-miterlimit" . psptFmt

-- | @ stroke-linejoin=\"...\" @
--
attr_stroke_linejoin :: LineJoin -> Doc
attr_stroke_linejoin = svgAttr "stroke-linejoin" . step 
  where
    step JoinMiter = text "miter"
    step JoinRound = text "round"
    step JoinBevel = text "bevel"


-- | @ stroke-linecap=\"...\" @
--
attr_stroke_linecap :: LineCap -> Doc
attr_stroke_linecap = svgAttr "stroke-linecap" . step
  where
    step CapButt   = text "butt"
    step CapRound  = text "round"
    step CapSquare = text "square"


-- | @ stroke-dasharray=\"...\" @
--
attr_stroke_dasharray :: [(Int,Int)] -> Doc
attr_stroke_dasharray = svgAttr "stroke-dasharray" . step 
  where
    step []         = empty
    step [(a,b)]    = int a <> comma <> int b 
    step ((a,b):xs) = int a <> comma <> int b <> step xs

-- | @ stroke-dasharray=\"none\" @
--
attr_stroke_dasharray_none :: Doc
attr_stroke_dasharray_none = svgAttr "stroke-dasharray" (text "none")

-- | @ stroke-dashoffset=\"...\" @
--
attr_stroke_dashoffset :: Int -> Doc
attr_stroke_dashoffset = svgAttr "stroke-dashoffset" . int

-- | @ clip_path="url(#...)" @
--
attr_clip_path :: String -> Doc
attr_clip_path ss = svgAttr "clip-path" (text "url" <> parens (text $ '#':ss)) 


-- | @ transform="..." @
--
attr_transform :: Doc -> Doc
attr_transform = svgAttr "transform"

-- | @ matrix(..., ..., ..., ..., ..., ...) @
--
val_matrix :: PtSize u => Matrix3'3 u -> Doc
val_matrix mtrx = text "matrix" <> tupled (map psptFmt [a,b,c,d,e,f])
  where
    (a,b,c,d,e,f) = deconsMatrix mtrx

-- | @ translate(..., ..., ..., ..., ..., ...) @
--
val_translate :: PtSize u => Vec2 u -> Doc
val_translate (V2 x y) = text "translate" <> tupled [psptFmt x, psptFmt y]
