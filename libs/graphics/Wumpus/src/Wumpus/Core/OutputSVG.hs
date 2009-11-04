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
pictureElt Empty                     = gElement [] []
pictureElt (Single (fr,_) prim)      = 
    gElement (frameChange fr) [svgPrimitive prim]
pictureElt (Multi (fr,_) ps)         = 
    gElement (frameChange fr) (map svgPrimitive ps)
pictureElt (Picture (fr,_) prop a b) = 
    gElement (frameChange fr) [pictureElt a, pictureElt b]

svgPrimitive :: Primitive Double -> Element
svgPrimitive (Path1 props p)           = pathElt props p
svgPrimitive (Label1 props l)          = labelElt props l
svgPrimitive (Ellipse1 (mbc,dp) c w h) = ellipseE dp c w h


pathElt :: PathProps -> Path Double -> Element
pathElt (mbc,_) (Path dp (P2 x y) xs) = 
    element_path ps # add_attrs [fillAttr mbc dp, strokeAttr mbc dp]
  where
    ps = pathDesc dp x y xs


labelElt :: LabelProps -> Label Double -> Element
labelElt (mbc,mbf) (Label (P2 x y) str) = 
     element_text str # add_attrs (mbCons ocattr $ attr_ls ++ fontattrs)
  where
    attr_ls   = [attr_x x, attr_y y]
    ocattr    = fmap attr_color mbc
    fontattrs = maybe [] fontAttrs mbf



mbCons :: Maybe a -> [a] -> [a]
mbCons Nothing  = id
mbCons (Just x) = (x:)
   
fillAttr :: Maybe PSColour -> DrawProp -> Attr
fillAttr mbc CFill = unqualAttr "fill" $ maybe "black" val_colour mbc
fillAttr _   _     = unqualAttr "fill" "none"


strokeAttr :: Maybe PSColour -> DrawProp -> Attr
strokeAttr mbc OStroke = unqualAttr "stroke" $ maybe "black" val_colour mbc
strokeAttr mbc CStroke = unqualAttr "stroke" $ maybe "black" val_colour mbc
strokeAttr _   _       = unqualAttr "stroke" "none"


-- Clipping to think about...


pathDesc :: DrawProp -> Double -> Double -> [PathSeg Double] -> [String]
pathDesc dp x y xs = 
    closepath dp $ "M": show x : show y : map pathSegDesc xs
  where 
    closepath OStroke = id
    closepath _       = (++ ["Z"])

   

pathSegDesc :: PathSeg Double -> String
pathSegDesc (PLine (P2 x y))  = hsep ["L", show x, show y]
pathSegDesc (PCurve p1 p2 p3) = hsep $ "L" : map show [x1,y1,x2,y2,x3,y3]
  where
    P2 x1 y1 = p1
    P2 x2 y2 = p2
    P2 x3 y3 = p3


ellipseE :: DrawProp -> Point2 Double -> Double -> Double -> Element
ellipseE _dp (P2 x y) w h 
    | w == h    = unode "circle"  [attr_x x, attr_y y, attr_rx w]
    | otherwise = unode "ellipse" [attr_x x, attr_y y, attr_rx w, attr_ry h]


frameChange :: Maybe (Frame2 Double) -> [Attr]
frameChange = maybe [] (return . matrixAttr)

fontAttrs :: FontAttr -> [Attr]
fontAttrs (FontAttr name sz) = 
  [unqualAttr "font-family" name, attr_fontsize  sz]

matrixAttr :: Frame2 Double -> Attr
matrixAttr fr = unqualAttr "transform" mstring where
    mstring         = "matrix" ++ tupled (map dtrunc [a,b,c,d,e,f])
    CTM a b c d e f = toCTM fr

translateAttr :: Double -> Double -> Attr
translateAttr tx ty = unqualAttr "transform" tstring where
   tstring = "translate" ++ tupled (map dtrunc [tx,ty])


coords :: Point2 Double -> (Attr,Attr)
coords (P2 x y) = (unqualAttr "cx" (show x), unqualAttr "cy" (show y))


