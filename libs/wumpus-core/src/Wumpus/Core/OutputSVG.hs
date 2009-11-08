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
import Wumpus.Core.Picture
import Wumpus.Core.SVG
import Wumpus.Core.Utils

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
    mkPic = runSVG . pictureElt False
    mkVec = snd . repositionProperties


prefixXmlDecls :: Element -> [Content]
prefixXmlDecls e = [Text xmlVersion, Text svgDocType, Elem e]    

topLevelPic :: Maybe (Vec2 Double) -> Element -> Element
topLevelPic Nothing         p = svgElement [p]
topLevelPic (Just (V2 x y)) p = svgElement [gElement [translateAttr x y] [p]]

pictureElt :: Clipped -> Picture Double -> SvgM Element
pictureElt _ Empty                   = return $ gElement [] []

pictureElt c (Single (fr,_) prim)    = do 
    elt <- svgPrimitive c prim
    return $ gElement [frameChange fr] [elt]

pictureElt c (Multi (fr,_) ps)       = do
    es <- mapM (svgPrimitive c) ps
    return $ gElement [frameChange fr] es

pictureElt c (Picture (fr,_) a b)    = do
    e1 <- pictureElt c a
    e2 <- pictureElt c b 
    return $ gElement [frameChange fr] [e1,e2]

pictureElt _ (Clip (fr,_) cp a) = do 
   lbl <- newClipLabel 
   e1  <- pictureElt True a
   return $ gElement [frameChange fr] [clipPathElt lbl cp,e1]


svgPrimitive :: Clipped -> Primitive Double -> SvgM Element
svgPrimitive c (Path1 props p)            = clipAttrib c $ pathElt props p
svgPrimitive c (Label1 props l)           = clipAttrib c $ labelElt props l
svgPrimitive c (Ellipse1 props mid hw hh) = clipAttrib c $ 
                                                ellipseE props mid hw hh

clipAttrib :: Clipped -> Element -> SvgM Element
clipAttrib False elt = return elt
clipAttrib True  elt = do 
    s <- currentClipLabel
    return $ add_attr (attr_clippath s) elt




pathElt :: PathProps -> Path Double -> Element
pathElt (c,dp) p = 
    element_path ps # add_attrs (fill_a : stroke_a : opts)
  where
    (fill_a,stroke_a,opts) = drawProperties c dp
    ps                     = pathDesc dp p 


clipPathElt :: String -> Path Double -> Element
clipPathElt name p = 
    element_clippath ps # add_attr (attr_id name)
  where
    ps = pathInstructions p ++ ["Z"]



labelElt :: LabelProps -> Label Double -> Element
labelElt (c,FontAttr _ fam sz) (Label pt str) = 
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
 


-- CFill   ==> stroke="none" fill="..."
-- CStroke ==> stroke="..."  fill="none"
-- OStroke ==> stroke="..."  fill="none"
--
-- A rule of thumb seems to be that SVG (at least SVG in Firefox)
-- will try to fill unless told not to. So always label paths
-- with @fill=...@ even if fill is @\"none\"@.



drawProperties :: PSColour -> DrawProp -> (Attr, Attr, [Attr])
drawProperties = fn where
  fn c CFill        = (attr_fill c, attr_stroke_none, [])
  fn c (OStroke xs) = (attr_fill_none, attr_stroke c, map strokeAttribute xs)
  fn c (CStroke xs) = (attr_fill_none, attr_stroke c, map strokeAttribute xs)
 

strokeAttribute :: StrokeAttr -> Attr
strokeAttribute (LineWidth a)    = attr_stroke_width a
strokeAttribute (MiterLimit a)   = attr_stroke_miterlimit a
strokeAttribute (LineCap lc)     = attr_stroke_linecap lc
strokeAttribute (LineJoin lj)    = attr_stroke_linejoin lj
strokeAttribute (DashPattern dp) = fn dp where
   fn Solid         = attr_stroke_dasharray_none
   fn (Dash _i _ns) = error $ "DashPattern not implemented yet..."
   


-- Clipping in PostScript works by changing the graphics state
-- Clip a path, then all subsequent drawing will only be rendered
-- when it is within the clip bounds. Clearly using clipping 
-- paths within a @gsave ... grestore@ block is a good idea...
--
-- SVG uses /tagging/. A clipPath element is declared and named 
-- then referenced in subsequent elements via the clip-path 
-- attribute - @clip-path=\"url(#clip_path_tag)\"@.
--


pathDesc :: DrawProp -> Path Double -> SvgPath
pathDesc dp p = close dp $ pathInstructions p
  where 
    close (OStroke _) = id
    close _           = closePath

pathInstructions :: Path Double -> [String]
pathInstructions (Path (P2 x y) xs) = path_m x y : map fn xs
  where 
    fn (PLine (P2 x1 y1))                        = path_l x1 y1
    fn (PCurve (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = path_s x1 y1 x2 y2 x3 y3


ellipseE :: EllipseProps -> Point2 Double -> Double -> Double -> Element
ellipseE _props (P2 x y) w h 
    | w == h    = element_circle # add_attrs  [attr_x x, attr_y y, attr_rx w]
    | otherwise = element_ellipse # add_attrs [attr_x x, attr_y y, attr_rx w, attr_ry h]


frameChange :: Frame2 Double -> Attr
frameChange fr = attr_transform $ val_matrix a b c d e f where
    CTM a b c d e f = toCTM fr

translateAttr :: Double -> Double -> Attr
translateAttr tx ty = unqualAttr "transform" tstring where
   tstring = "translate" ++ tupled (map dtrunc [tx,ty])



closePath :: SvgPath -> SvgPath 
closePath xs = xs ++ ["Z"]