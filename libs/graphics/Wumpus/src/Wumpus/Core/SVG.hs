{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.SVG
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.SVG
  where
--  ( 
--  
--  -- * Output SVG
--  , writeSVG
--  ) where

import Wumpus.Core.Colour
import Wumpus.Core.GraphicsState
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

svgElement :: [Element] -> Element
svgElement xs = unode "svg" ([xmlns,version],xs)
  where
    xmlns   = unqualAttr "xmlns" "http://www.w3.org/2000/svg"
    version = unqualAttr "version" "1.1"  


--------------------------------------------------------------------------------



-- | @ rx=\"...\" @
attr_x :: Double -> Attr
attr_x      = unqualAttr "x" . dtrunc

-- | @ ry=\"...\" @
attr_y :: Double -> Attr
attr_y      = unqualAttr "y" . dtrunc


-- | @ rx=\"...\" @
attr_rx :: Double -> Attr
attr_rx      = unqualAttr "rx" . dtrunc

-- | @ ry=\"...\" @
attr_ry :: Double -> Attr
attr_ry      = unqualAttr "ry" . dtrunc


element_path :: [String] -> Element
element_path = unode "path" . unqualAttr "d" . hsep

element_text :: String -> Element
element_text = unode "text" . content_text


content_text :: String -> Content
content_text str = Text $ CData CDataText str Nothing


-- | @ font-family=\"...\" @
attr_fontfamily :: String -> Attr
attr_fontfamily = unqualAttr "font-family" 

-- | @ font-size=\"...\" @
attr_fontsize :: Int -> Attr
attr_fontsize = unqualAttr "font-size" . show



-- | @ fill=\"rgb(..., ..., ...)\" @
attr_fill :: PSColour -> Attr
attr_fill = unqualAttr "fill" . val_colour


attr_color :: PSColour -> Attr
attr_color = unqualAttr "color" . val_colour

-- | @ rgb(..., ..., ...) @
-- 
-- HSB and gray scale are translated to RGB values.
val_colour :: PSColour -> String
val_colour (PSRgb r g b) = val_rgb $ RGB3 r g b
val_colour (PSHsb h s b) = val_rgb $ hsb2rgb $ HSB3 h s b
val_colour (PSGray a)    = val_rgb $ gray2rgb a


-- | @ rgb(..., ..., ...) @
val_rgb :: RGB3 Double -> String
val_rgb (RGB3 r g b) = "rgb" ++ show (range255 r,range255 g,range255 b)


-- | c.f. PostScript's @moveto@.
path_m :: Double -> Double -> String
path_m x y  = hsep $ "M" : map dtrunc [x,y]

-- | c.f. PostScript's @lineto@.
path_l :: Double -> Double -> String
path_l x y  = hsep $ "L" : map dtrunc [x,y]

-- | c.f. PostScript's @curveto@.
path_s :: Double -> Double -> Double -> Double -> Double -> Double -> String
path_s x1 y1 x2 y2 x3 y3 =  hsep $ "S" : map dtrunc [x1,y1,x2,y2,x3,y3]
