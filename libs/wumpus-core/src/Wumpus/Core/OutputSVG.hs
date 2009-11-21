{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.OutputSVG
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Output SVG. 
--
-- This is complicated by two differences with PostScript.
--
-- 1. The coordinate space of SVG is /origin top-left/, for 
-- PostScript it is /origin bottom-left/.
-- 
-- 2. Clipping in PostScript works by changing the graphics state
-- Clip a path, then all subsequent drawing be rendered only 
-- when it is within the clip bounds. Clearly using clipping 
-- paths within a @gsave ... grestore@ block is a good idea...
--
-- SVG uses /tagging/. A clipPath element is declared and named 
-- then referenced in subsequent elements via the clip-path 
-- attribute - @clip-path=\"url(#clip_path_tag)\"@.
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.OutputSVG
  ( 
  
  -- * Output SVG
    writeSVG
  
  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.PictureInternal
import Wumpus.Core.SVG
import Wumpus.Core.TextEncoding
import Wumpus.Core.Utils

import Data.Aviary ( (#), bigphi )

import Text.XML.Light



type Clipped    = Bool


coordChange ::  (Num u, Ord u, Scale t, u ~ DUnit t) => t -> t
coordChange = scale 1 (-1)


--------------------------------------------------------------------------------

writeSVG :: (Ord u, PSUnit u) => FilePath -> Picture u -> IO ()
writeSVG filepath pic = 
    writeFile filepath $ unlines $ map ppContent $ svgDraw pic 


svgDraw :: (Ord u, PSUnit u) => Picture u -> [Content]
svgDraw = prefixXmlDecls . bigphi topLevelPic mkVec mkPic . coordChange 
  where
    mkPic = runSVG . picture False
    mkVec = snd . repositionProperties


prefixXmlDecls :: Element -> [Content]
prefixXmlDecls e = [Text xmlVersion, Text svgDocType, Elem e]    

topLevelPic :: PSUnit u => Maybe (Vec2 u) -> Element -> Element
topLevelPic Nothing         p = svgElement [p]
topLevelPic (Just (V2 x y)) p = svgElement [gElement [trans_attr] [p]] 
  where 
    trans_attr = attr_transform $ val_translate x y



picture :: (Ord u, PSUnit u) => Clipped -> Picture u -> SvgM Element
picture _ (PicBlank _)            = return $ gElement [] []
picture c (Single (fr,_) prim)    = do 
    elt <- primitive c prim
    return $ gElement (maybe [] return $ frameChange fr) [elt]

picture c (Picture (fr,_) ones)    = do
    es <- toListWithM (picture c) ones
    return $ gElement (maybe [] return $ frameChange fr) es

picture _ (Clip (fr,_) p a) = do 
   cp <- clipPath p
   e1 <- picture True a
   return $ gElement (maybe [] return $ frameChange fr) [cp,e1]


primitive :: (Ord u, PSUnit u) => Clipped -> Primitive u -> SvgM Element
primitive c (PPath props p)            = clipAttrib c $ path props p
primitive c (PLabel props l)           = clipAttrib c $ label props l
primitive c (PEllipse props mid hw hh) = clipAttrib c $ 
                                                ellipse props mid hw hh



-- All clipping paths are closed.
clipPath :: PSUnit u => Path u -> SvgM Element
clipPath p = do
    name <- newClipLabel
    return $ element_clippath ps # add_attr (attr_id name)
  where
    ps = closePath $ pathInstructions p



clipAttrib :: Clipped -> Element -> SvgM Element
clipAttrib False elt = return elt
clipAttrib True  elt = do 
    s <- currentClipLabel
    return $ add_attr (attr_clippath s) elt


-- None of the remaining translation functions need to be in the
-- SvgM monad.

path :: PSUnit u => PathProps -> Path u -> Element
path (c,dp) p = 
    element_path ps # add_attrs (fill_a : stroke_a : opts)
  where
    (fill_a,stroke_a,opts) = drawProperties c dp
    ps                     = svgPath dp p 


-- Labels need the coordinate system remapping otherwise
-- the will be printed upside down. Both the start point and 
-- the label itself need transforming.
-- 
-- Also rendering coloured text is convoluted (needing the
-- tspan element).
-- 
label :: (Ord u, PSUnit u) => LabelProps -> Label u -> Element
label (c,FontAttr _ fam style sz) (Label pt entxt) = 
     element_text tspan_elt # add_attrs text_xs # add_attrs (fontStyle style)
  where
    P2 x y    = coordChange pt
    text_xs   = [ attr_x x
                , attr_y y 
                , attr_transform $ val_matrix 1 0 0 (-1) 0 (0::Double)
                , attr_font_family fam
                , attr_font_size sz 
                ]
    tspan_elt = element_tspan str # add_attrs [ attr_fill c ]
    
    str       = encodedText entxt


encodedText :: EncodedText -> String 
encodedText = concat . map textChunk . getEncodedText

textChunk :: TextChunk -> String
textChunk (SText s)  = s
textChunk (EscInt i) = "#&" ++ show i ++ ";"
textChunk (EscStr _s) = "" -- TODO


 
fontStyle :: SVGFontStyle -> [Attr]
fontStyle SVG_REGULAR      = []
fontStyle SVG_BOLD         = [attr_font_weight "bold"]
fontStyle SVG_ITALIC       = [attr_font_style "italic"]
fontStyle SVG_BOLD_ITALIC  = 
    [attr_font_weight "bold", attr_font_style "italic"]
fontStyle SVG_OBLIQUE      = [attr_font_style "oblique"]
fontStyle SVG_BOLD_OBLIQUE = 
    [attr_font_weight "bold", attr_font_style "oblique"]

-- If w==h the draw the ellipse as a circle

ellipse :: PSUnit u => EllipseProps -> Point2 u -> u -> u -> Element
ellipse (c,dp) (P2 x y) w h 
    | w == h    = element_circle  # add_attrs (circle_attrs  ++ style_attrs)
    | otherwise = element_ellipse # add_attrs (ellipse_attrs ++ style_attrs)
  where
    circle_attrs  = [attr_cx x, attr_cy y, attr_r w]
    ellipse_attrs = [attr_cx x, attr_cy y, attr_rx w, attr_ry h]
    style_attrs   = fill_a : stroke_a : opts
                    where (fill_a,stroke_a,opts) = drawEllipse c dp


-- A rule of thumb seems to be that SVG (at least SVG in Firefox)
-- will try to fill unless told not to. So always label paths
-- with @fill=...@ even if fill is @\"none\"@.
--
-- CFill   ==> stroke="none" fill="..."
-- CStroke ==> stroke="..."  fill="none"
-- OStroke ==> stroke="..."  fill="none"
--

drawProperties :: PSColour c => c -> DrawProp -> (Attr, Attr, [Attr])
drawProperties = fn where
  fn c CFill        = (attr_fill c, attr_stroke_none, [])
  fn c (OStroke xs) = (attr_fill_none, attr_stroke c, map strokeAttribute xs)
  fn c (CStroke xs) = (attr_fill_none, attr_stroke c, map strokeAttribute xs)

drawEllipse :: PSColour c => c -> DrawEllipse -> (Attr, Attr, [Attr])
drawEllipse = fn where
  fn c EFill        = (attr_fill c, attr_stroke_none, [])
  fn c (EStroke xs) = (attr_fill_none, attr_stroke c, map strokeAttribute xs)
 

strokeAttribute :: StrokeAttr -> Attr
strokeAttribute (LineWidth a)    = attr_stroke_width a
strokeAttribute (MiterLimit a)   = attr_stroke_miterlimit a
strokeAttribute (LineCap lc)     = attr_stroke_linecap lc
strokeAttribute (LineJoin lj)    = attr_stroke_linejoin lj
strokeAttribute (DashPattern dp) = fn dp where
   fn Solid         = attr_stroke_dasharray_none
   fn (Dash _i _ns) = error $ "DashPattern not implemented yet..."
   


svgPath :: PSUnit u => DrawProp -> Path u -> SvgPath
svgPath (OStroke _) p = pathInstructions p
svgPath _           p = closePath $ pathInstructions p


pathInstructions :: PSUnit u => Path u -> [String]
pathInstructions (Path (P2 x y) xs) = path_m x y : map pathSegment xs

pathSegment :: PSUnit u => PathSegment u -> String
pathSegment (PLine (P2 x1 y1))                        = path_l x1 y1
pathSegment (PCurve (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = path_s x1 y1 x2 y2 x3 y3



frameChange :: PSUnit u => Frame2 u -> Maybe Attr
frameChange fr 
    | standardFrame fr = Nothing
    | otherwise        = Just $ attr_transform $ val_matrix a b c d e f 
  where
    CTM a b c d e f = toCTM fr



closePath :: SvgPath -> SvgPath 
closePath xs = xs ++ ["Z"]