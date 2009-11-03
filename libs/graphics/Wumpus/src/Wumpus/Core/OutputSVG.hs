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

hsep :: [String] -> String
hsep = concat . intersperse " "


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
gElement :: [Element] -> Element
gElement = unode "g" 

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
svgDraw p = [Text xmlVersion, Text svgDocType, svgpic] 
  where
    svgpic = Elem $ svgElement 0 0 200 200 [svgPicture p]

svgPicture :: Picture Double -> Element
svgPicture Empty                     = gElement []
svgPicture (Single (_fr,_) prim)     = gElement [svgPrimitive prim]

svgPicture (Multi (_fr,_) ps)        = gElement $ map svgPrimitive ps

svgPicture _                         = error "svgPicture"

{-
svgPicture (Picture (fr,_) prop a b) = updatePen prop $ do
    updateFrame fr $ outputPicture  a
    updateFrame fr $ outputPicture  b

-}

svgPrimitive :: Primitive Double -> Element
svgPrimitive (Path1 props p)           = svgPath props p
svgPrimitive _                         = error "svgPrimitive"

{-
svgPrimitive (Label1 props l)          = ...
svgPrimitive (Ellipse1 (mbc,dp) c w h) = ...
-}


svgPath :: PathProps -> Path Double -> Element
svgPath (mbc,_) (Path dp (P2 x y) xs) = 
    unode "path" [d,fill mbc dp, stroke mbc dp] 
  where
    d    = unqualAttr "d" (pathString dp x y xs)
   
fill :: Maybe PSColour -> DrawProp -> Attr
fill mbc CFill = unqualAttr "fill" $ maybe "black" colourDesc mbc
fill _   _     = unqualAttr "fill" "none"


stroke :: Maybe PSColour -> DrawProp -> Attr
stroke mbc OStroke = unqualAttr "stroke" $ maybe "black" colourDesc mbc
stroke mbc CStroke = unqualAttr "stroke" $ maybe "black" colourDesc mbc
stroke _   _       = unqualAttr "stroke" "none"


-- Clipping to think about...


pathString :: DrawProp -> Double -> Double -> [PathSeg Double] -> String
pathString dp x y xs = 
    hsep $ closepath dp $ "M": show x : show y : map cmdPathSeg xs
  where 
    closepath OStroke = id
    closepath _       = (++ ["Z"])

   

cmdPathSeg :: PathSeg Double -> String
cmdPathSeg (PLine (P2 x y))  = hsep ["L", show x, show y]
cmdPathSeg (PCurve p1 p2 p3) = hsep $ "L" : map show [x1,y1,x2,y2,x3,y3]
  where
    P2 x1 y1 = p1
    P2 x2 y2 = p2
    P2 x3 y3 = p3


svgEllipse :: DrawProp -> Point2 Double -> Double -> Double -> Element
svgEllipse _dp p w h 
    | w == h    = unode "circle"  [cx,cy,rx]
    | otherwise = unode "ellipse" [cx,cy,rx,ry]
  where
    (cx,cy) = coords p
    rx      = unqualAttr "rx" $ show w
    ry      = unqualAttr "ry" $ show h
     

colourDesc :: PSColour -> String
colourDesc (PSRgb r g b) = rgbDesc $ RGB3 r g b
colourDesc (PSHsb h s b) = rgbDesc $ hsb2rgb $ HSB3 h s b
colourDesc (PSGray a)    = rgbDesc $ gray2rgb a

rgbDesc :: RGB3 Double -> String
rgbDesc (RGB3 r g b) = "rgb" ++ show (range255 r,range255 g,range255 b)

coords :: Point2 Double -> (Attr,Attr)
coords (P2 x y) = (unqualAttr "cx" (show x), unqualAttr "cy" (show y))


range255 :: Double -> Int
range255 = floor . (*255)