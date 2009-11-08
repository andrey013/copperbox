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
import Wumpus.Core.Picture hiding ( ellipse )
import Wumpus.Core.SVG

import Data.FunctionExtras ( (#), subst' )

import Text.XML.Light


type Clipped    = Bool


coordChange ::  (Num u, Ord u, Scale t, u ~ ScaleUnit t) => t-> t
coordChange = scale 1 (-1)


--------------------------------------------------------------------------------

writeSVG :: FilePath -> Picture Double -> IO ()
writeSVG filepath pic = 
    writeFile filepath $ unlines $ map ppContent $ svgDraw pic 


svgDraw :: Picture Double -> [Content]
svgDraw = prefixXmlDecls . subst' topLevelPic mkVec mkPic . coordChange 
  where
    mkPic = runSVG . picture False
    mkVec = snd . repositionProperties


prefixXmlDecls :: Element -> [Content]
prefixXmlDecls e = [Text xmlVersion, Text svgDocType, Elem e]    

topLevelPic :: Maybe (Vec2 Double) -> Element -> Element
topLevelPic Nothing         p = svgElement [p]
topLevelPic (Just (V2 x y)) p = svgElement [gElement [trans_attr] [p]] 
  where 
    trans_attr = attr_transform $ val_translate x y



picture :: Clipped -> Picture Double -> SvgM Element
picture _ Empty                   = return $ gElement [] []

picture c (Single (fr,_) prim)    = do 
    elt <- primitive c prim
    return $ gElement [frameChange fr] [elt]

picture c (Multi (fr,_) ps)       = do
    es <- mapM (primitive c) ps
    return $ gElement [frameChange fr] es

picture c (Picture (fr,_) a b)    = do
    e1 <- picture c a
    e2 <- picture c b 
    return $ gElement [frameChange fr] [e1,e2]

picture _ (Clip (fr,_) p a) = do 
   cp <- clipPath p
   e1 <- picture True a
   return $ gElement [frameChange fr] [cp,e1]


primitive :: Clipped -> Primitive Double -> SvgM Element
primitive c (Path1 props p)            = clipAttrib c $ path props p
primitive c (Label1 props l)           = clipAttrib c $ label props l
primitive c (Ellipse1 props mid hw hh) = clipAttrib c $ 
                                                ellipse props mid hw hh



-- All clipping paths are closed.
clipPath :: Path Double -> SvgM Element
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

path :: PathProps -> Path Double -> Element
path (c,dp) p = 
    element_path ps # add_attrs (fill_a : stroke_a : opts)
  where
    (fill_a,stroke_a,opts) = drawProperties c dp
    ps                     = svgPath dp p 


-- Labels need the coordinate system remapping otherwise
-- the will be printed upside down. Both the start point and 
-- the label itself need transforming.

label :: LabelProps -> Label Double -> Element
label (c,FontAttr _ fam sz) (Label pt str) = 
     element_text str # add_attrs xs
  where
    P2 x y = coordChange pt
    xs = [ attr_x x
         , attr_y y 
         , attr_transform $ val_matrix 1 0 0 (-1) 0 0
         , attr_color c
         , attr_fontfamily fam
         , attr_fontsize sz 
         ]
 

-- If w==h the draw the ellipse as a circle

ellipse :: EllipseProps -> Point2 Double -> Double -> Double -> Element
ellipse (c,dp) (P2 x y) w h 
    | w == h    = element_circle  # add_attrs (circle_attrs  ++ style_attrs)
    | otherwise = element_ellipse # add_attrs (ellipse_attrs ++ style_attrs)
  where
    circle_attrs  = [attr_x x, attr_y y, attr_rx w]
    ellipse_attrs = [attr_x x, attr_y y, attr_rx w, attr_ry h]
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

drawProperties :: PSColour -> DrawProp -> (Attr, Attr, [Attr])
drawProperties = fn where
  fn c CFill        = (attr_fill c, attr_stroke_none, [])
  fn c (OStroke xs) = (attr_fill_none, attr_stroke c, map strokeAttribute xs)
  fn c (CStroke xs) = (attr_fill_none, attr_stroke c, map strokeAttribute xs)

drawEllipse :: PSColour -> DrawEllipse -> (Attr, Attr, [Attr])
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
   


svgPath :: DrawProp -> Path Double -> SvgPath
svgPath (OStroke _) p = pathInstructions p
svgPath _           p = closePath $ pathInstructions p


pathInstructions :: Path Double -> [String]
pathInstructions (Path (P2 x y) xs) = path_m x y : map fn xs
  where 
    fn (PLine (P2 x1 y1))                        = path_l x1 y1
    fn (PCurve (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = path_s x1 y1 x2 y2 x3 y3



frameChange :: Frame2 Double -> Attr
frameChange fr = attr_transform $ val_matrix a b c d e f where
    CTM a b c d e f = toCTM fr



closePath :: SvgPath -> SvgPath 
closePath xs = xs ++ ["Z"]