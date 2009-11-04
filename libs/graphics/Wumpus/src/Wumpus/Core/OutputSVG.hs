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
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.Picture
import Wumpus.Core.PostScript
import Wumpus.Core.Utils

import Text.XML.Light

import Data.List ( intersperse )



--------------------------------------------------------------------------------
-- Helpers for XML.Light

unqualAttr :: String -> String -> Attr
unqualAttr name val = Attr (unqual name) val

parens :: String -> String
parens s = "(" ++ s  ++ ")"

hsep :: [String] -> String
hsep = concat . intersperse " "

tupled :: [String] -> String
tupled = parens . concat . intersperse ", " 

--------------------------------------------------------------------------------
-- SVG helpers


xmlVersion :: CData
xmlVersion = CData CDataRaw 
                   "<?xml version=\"1.0\" standalone=\"yes\"?>" 
                   (Just 1)

svgDocType :: CData
svgDocType = CData CDataRaw (line1 ++ "\n" ++ line2) (Just 1)
  where
    line1 = "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
    line2 = "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"

-- <g> ... </g> is considered equaivalent to gsave ... grestore  
gElement :: [Attr] -> [Element] -> Element
gElement xs ys = unode "g" (xs,ys)

svgElement :: Double -> Double -> Double -> Double -> [Element] -> Element
svgElement llx lly urx ury xs = unode "svg" ([xmlns,vbox,version],xs)
  where
    xmlns   = unqualAttr "xmlns" "http://www.w3.org/2000/svg"
    vbox    = unqualAttr "viewBox" $ hsep $ map dtrunc [llx,lly,urx,ury] 
    version = unqualAttr "version" "1.1"  


--------------------------------------------------------------------------------

writeSVG :: FilePath -> Picture Double -> IO ()
writeSVG filepath pic = 
    writeFile filepath $ unlines $ map ppContent $ svgDraw pic 

svgDraw :: Picture Double -> [Content]
svgDraw pic = [Text xmlVersion, Text svgDocType, svgpic] 
  where
    svgpic    = Elem $ svgElement llx lly urx ury [pic_elt]
    pic_elt   = add_attrs trans $ pictureElt pic
    bb0       = if nullPicture pic then BBox zeroPt zeroPt 
                                  else extractBounds pic
    (mbTx,bb) = translateBBox bb0
    trans     = maybe [] (\(x,y) -> [translateAttr x y]) mbTx
    (llx,lly,urx,ury) = lowerLeftUpperRight (0,0,0,0) bb


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
    unode "path" [d,fillAttr mbc dp, strokeAttr mbc dp] 
  where
    d    = unqualAttr "d" (pathDesc dp x y xs)


labelElt :: LabelProps -> Label Double -> Element
labelElt (mbc,mbf) (Label (P2 x y) str) = 
    unode "text" (mbCons ocattr $ [xattr,yattr] ++ fontattrs, textct)
  where
    xattr     = unqualAttr "x" $ show x
    yattr     = unqualAttr "y" $ show y
    textct    = Text $ CData CDataText str Nothing
    ocattr    = fmap colourAttr mbc
    fontattrs = maybe [] fontAttrs mbf



mbCons :: Maybe a -> [a] -> [a]
mbCons Nothing  = id
mbCons (Just x) = (x:)
   
fillAttr :: Maybe PSColour -> DrawProp -> Attr
fillAttr mbc CFill = unqualAttr "fill" $ maybe "black" colourDesc mbc
fillAttr _   _     = unqualAttr "fill" "none"


strokeAttr :: Maybe PSColour -> DrawProp -> Attr
strokeAttr mbc OStroke = unqualAttr "stroke" $ maybe "black" colourDesc mbc
strokeAttr mbc CStroke = unqualAttr "stroke" $ maybe "black" colourDesc mbc
strokeAttr _   _       = unqualAttr "stroke" "none"


-- Clipping to think about...


pathDesc :: DrawProp -> Double -> Double -> [PathSeg Double] -> String
pathDesc dp x y xs = 
    hsep $ closepath dp $ "M": show x : show y : map pathSegDesc xs
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
ellipseE _dp p w h 
    | w == h    = unode "circle"  [cx,cy,rx]
    | otherwise = unode "ellipse" [cx,cy,rx,ry]
  where
    (cx,cy) = coords p
    rx      = unqualAttr "rx" $ show w
    ry      = unqualAttr "ry" $ show h

colourAttr :: PSColour -> Attr
colourAttr = unqualAttr "color" . colourDesc


colourDesc :: PSColour -> String
colourDesc (PSRgb r g b) = rgbDesc $ RGB3 r g b
colourDesc (PSHsb h s b) = rgbDesc $ hsb2rgb $ HSB3 h s b
colourDesc (PSGray a)    = rgbDesc $ gray2rgb a

rgbDesc :: RGB3 Double -> String
rgbDesc (RGB3 r g b) = "rgb" ++ show (range255 r,range255 g,range255 b)

frameChange :: Maybe (Frame2 Double) -> [Attr]
frameChange = maybe [] (return . matrixAttr)

fontAttrs :: FontAttr -> [Attr]
fontAttrs (FontAttr name sz) = 
  [unqualAttr "font-family" name, unqualAttr "font-size" (show sz)]

matrixAttr :: Frame2 Double -> Attr
matrixAttr fr = unqualAttr "transform" mstring where
    mstring         = "matrix" ++ tupled (map dtrunc [a,b,c,d,e,f])
    CTM a b c d e f = toCTM fr

translateAttr :: Double -> Double -> Attr
translateAttr tx ty = unqualAttr "transform" tstring where
   tstring = "translate" ++ tupled (map dtrunc [tx,ty])


fontSizeAttr :: Int -> Attr
fontSizeAttr = unqualAttr "font-size" . show

coords :: Point2 Double -> (Attr,Attr)
coords (P2 x y) = (unqualAttr "cx" (show x), unqualAttr "cy" (show y))


range255 :: Double -> Int
range255 = floor . (*255)