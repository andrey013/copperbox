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
  where
--  ( 
--  
--  -- * Output SVG
--  , writeSVG
--  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.Picture
import Wumpus.Core.SVG
import Wumpus.Core.Utils

import Data.FunctionExtras ( (#) )

import Text.XML.Light




--------------------------------------------------------------------------------

writeSVG :: FilePath -> Picture Double -> IO ()
writeSVG filepath pic = 
    writeFile filepath $ unlines $ map ppContent $ svgDraw pic 


svgDraw :: Picture Double -> [Content]
svgDraw pic = [Text xmlVersion, Text svgDocType, svgpic] 
  where
    svgpic    = Elem $ svgElement [pic_elt]
    pic_elt   = gElement trans [pictureElt pic]
    bb0       = if nullPicture pic then BBox zeroPt zeroPt 
                                   else extractBounds pic
    (mbTx,_)  = translateBBox bb0
    trans     = maybe [] (\(x,y) -> [translateAttr x y]) mbTx
    

pictureElt :: Picture Double -> Element
pictureElt Empty                   = gElement [] []
pictureElt (Single (fr,_) prim)    = 
    gElement [frameChange fr] [svgPrimitive prim]
pictureElt (Multi (fr,_) ps)       = 
    gElement [frameChange fr] (map svgPrimitive ps)
pictureElt (Picture (fr,_) a b)    = 
    gElement [frameChange fr] [pictureElt a, pictureElt b]
pictureElt (Clip (_fr,_) _path _p) = error "Clip TODO" 


svgPrimitive :: Primitive Double -> Element
svgPrimitive (Path1 props p)           = pathElt props p
svgPrimitive (Label1 props l)          = labelElt props l
svgPrimitive (Ellipse1 (_c,dp) mid w h) = ellipseE dp mid w h


pathElt :: PathProps -> Path Double -> Element
pathElt (c,_,dp) (Path (P2 x y) xs) = 
    element_path ps # add_attrs [fillAttr c dp, strokeAttr c dp]
  where
    ps = pathDesc dp x y xs


labelElt :: LabelProps -> Label Double -> Element
labelElt (c,FontAttr _ fam sz) (Label (P2 x y) str) = 
     element_text str # add_attrs xs
  where
    xs = [ attr_x x, attr_y y, attr_color c, 
           attr_fontfamily fam, 
           attr_fontsize sz ]
 


-- CFill   ==> stroke="none" fill="..."
-- CStroke ==> stroke="..."  fill="none"
-- OStroke ==> stroke="..."  fill="none"
--
-- A rule of thumb seems to be that SVG (at least SVG in Firefox)
-- will try to fill unless told not to. So always label paths
-- with @fill=...@ even if fill is @\"none\"@.
   
fillAttr :: PSColour -> DrawProp -> Attr
fillAttr c CFill = unqualAttr "fill" $ val_colour c
fillAttr _   _   = unqualAttr "fill" "none"


strokeAttr :: PSColour -> DrawProp -> Attr
strokeAttr c OStroke = unqualAttr "stroke" $ val_colour c
strokeAttr c CStroke = unqualAttr "stroke" $ val_colour c
strokeAttr _ _       = unqualAttr "stroke" "none"


-- Clipping to think about...
--
-- Clipping in PostScript works by changing the graphics state
-- Clip a path, then all subsequent drawing will only be rendered
-- when it is within the clip bounds. Clearly using clipping 
-- paths within a @gsave ... grestore@ block is a good idea. This 
-- is what Wumpus does. In some respects this clipping might be 
-- considered implicit. 
--
-- SVG uses /tagging/. A clipPath element is declared and named 
-- then referenced in subsequent elements via the clip-path 
-- attribute - @clip-path=\"url(#clip_path_tag)\"@.
--
-- Vis-a-vis the picture language, there is a good argument to 
-- make a clipping path one of the constructors of the Picture 
-- type 
-- 
-- >   | Clip ... (Path u) (Picture u)
--
-- (->-) etc. on clipped pictures then have obvious meaning.
-- Also the PostScript implementation would be no harder, and the 
-- SVG implementation should be easier.
--
-- UPDATE: Picture - been changed to match the above, which
-- obliges the SVG output to go monadic so it can handle a 
-- counter. This hasn't been done yet.


pathDesc :: DrawProp -> Double -> Double -> [PathSeg Double] -> [String]
pathDesc dp x y xs = close dp $ path_m x y : map fn xs
  where 
    fn (PLine (P2 x1 y1))                        = path_l x1 y1
    fn (PCurve (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = path_s x1 y1 x2 y2 x3 y3

    close OStroke = id
    close _       = (++ ["Z"])



ellipseE :: DrawProp -> Point2 Double -> Double -> Double -> Element
ellipseE _dp (P2 x y) w h 
    | w == h    = unode "circle"  [attr_x x, attr_y y, attr_rx w]
    | otherwise = unode "ellipse" [attr_x x, attr_y y, attr_rx w, attr_ry h]


frameChange :: Frame2 Double -> Attr
frameChange = matrixAttr

matrixAttr :: Frame2 Double -> Attr
matrixAttr fr = unqualAttr "transform" mstring where
    mstring         = "matrix" ++ tupled (map dtrunc [a,b,c,d,e,f])
    CTM a b c d e f = toCTM fr

translateAttr :: Double -> Double -> Attr
translateAttr tx ty = unqualAttr "transform" tstring where
   tstring = "translate" ++ tupled (map dtrunc [tx,ty])

