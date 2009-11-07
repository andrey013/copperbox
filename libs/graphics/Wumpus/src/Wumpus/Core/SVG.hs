{-# LANGUAGE MultiParamTypeClasses      #-}
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

import MonadLib hiding ( version )
import Text.XML.Light

import Data.List ( intersperse )

data SvgState = SvgSt { clipCount :: Int }

type SvgM a = SvgT Id a

newtype SvgT m a = SvgT { unSvgT :: StateT SvgState m a }

runSvgT :: Monad m => SvgT m a -> m (a,SvgState)
runSvgT m = runStateT st0 (unSvgT m) where
    st0 = SvgSt { clipCount = 0 } 

instance Monad m => Functor (SvgT m) where
  fmap f (SvgT mf) = SvgT $ fmap f mf 

instance Monad m => Monad (SvgT m) where
  return a  = SvgT $ return a
  ma >>= f  = SvgT $ unSvgT ma >>= unSvgT . f

instance Monad m => StateM (SvgT m) SvgState where
  get = SvgT $ get
  set = SvgT . set

instance MonadT SvgT where
  lift = SvgT . lift


svgId :: SvgT Id a -> (a,SvgState)
svgId m = runId $ runSvgT m  


runSVG :: SvgM a -> a
runSVG = fst . svgId


currentClipLabel :: SvgM String
currentClipLabel = get >>= return . clipname . clipCount


newClipLabel :: SvgM String
newClipLabel = do 
  i <- (get >>= return . clipCount)
  sets_ (\s -> s { clipCount=i+1 })
  return $ clipname i


clipname :: Int -> String
clipname = ("clip" ++) . show


--------------------------------------------------------------------------------
-- Helpers for XML.Light and /data in strings/.

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

element_clippath :: [String] -> Element
element_clippath = unode "clipPath" . element_path

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


-- | @ id=\"...\" @
attr_id :: String -> Attr
attr_id = unqualAttr "id" 


-- | @ fill=\"rgb(..., ..., ...)\" @
attr_fill :: PSColour -> Attr
attr_fill = unqualAttr "fill" . val_colour


attr_color :: PSColour -> Attr
attr_color = unqualAttr "color" . val_colour

-- | @ clip-path=\"url(#...)\" @
attr_clippath :: String -> Attr
attr_clippath = unqualAttr "clip-path" . val_url

-- | @ transform="..." @
attr_transform :: String -> Attr
attr_transform = unqualAttr "transform"

-- | @ matrix(..., ..., ..., ..., ..., ...) @
val_matrix :: Double -> Double -> Double 
           -> Double -> Double -> Double -> String
val_matrix a b c d e f = "matrix" ++ tupled (map dtrunc [a,b,c,d,e,f])


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


-- | @ url(#...) @
val_url :: String -> String
val_url s = "url" ++ parens ('#':s)
  
-- | c.f. PostScript's @moveto@.
path_m :: Double -> Double -> String
path_m x y  = hsep $ "M" : map dtrunc [x,y]

-- | c.f. PostScript's @lineto@.
path_l :: Double -> Double -> String
path_l x y  = hsep $ "L" : map dtrunc [x,y]

-- | c.f. PostScript's @curveto@.
path_s :: Double -> Double -> Double -> Double -> Double -> Double -> String
path_s x1 y1 x2 y2 x3 y3 =  hsep $ "S" : map dtrunc [x1,y1,x2,y2,x3,y3]
