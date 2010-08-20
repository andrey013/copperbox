{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.SVG
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- SVG generation.
--
-- SVG is represented using XML.Light. XML.Light is a simple,
-- generic XML representation (almost) everything is an element 
-- with attributes.
--
-- SVG output is monadic to handle clipping paths and 
-- configurable text encoding via a Reader monad. 
--
-- SVG does not achieve clipping by changing the graphics state 
-- (being /declarative/ SVG doesn\'t have a graphics state as 
-- such). Instead a clipping path has an id, subsequent elements 
-- that are bound by the clipping path are tagged with a 
-- @clip-path@ attribute that references the clipping path id: 
--
-- > clip-path=\"url(#clip1)\"
-- 
-- 
-- The operations to build XML elements (e.g. element_path) don\'t 
-- take more parameters than necessary, and are expected to be 
-- augmented with attributes using 'add_attr' and 'add_attrs' from 
-- the XML.Light library.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.SVG 
  (
  -- * SVG Monad 
    SvgMonad
  , execSvgMonad
  , newClipLabel
  , currentClipLabel   
  , set 
  , get
  , ask
  , asks

  -- * Build SVG
  , SvgPath

  , unqualAttr
  , xmlVersion
  , svgDocType
  , gElement
  , svgElement
  
  , element_circle
  , element_ellipse
  , attr_x
  , attr_y
  , attr_r
  , attr_rx
  , attr_ry
  , attr_cx
  , attr_cy
  , element_path
  , element_clippath
  , element_text
  , element_tspan
  , content_text
  , attr_font_family
  , attr_font_size
  , attr_font_weight
  , attr_font_style
  , attr_id
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

  , attr_color
  , attr_clippath
  , attr_transform
  , val_matrix
  , val_colour
  , val_rgb
  , val_url
  , val_translate
  , path_m
  , path_l
  , path_c


  ) where

import Wumpus.Core.Colour
import Wumpus.Core.GraphicsState
import Wumpus.Core.TextEncoder
import Wumpus.Core.Utils

import Text.XML.Light                           -- package: xml


import Control.Applicative


data St = St { clipCount :: Int }

st_zero :: St
st_zero = St { clipCount = 0 } 

-- | The SVG monad - which wraps a state monad to generate 
-- fresh names.

newtype SvgMonad a = SvgMonad { 
          getSvgMonad :: TextEncoder -> St -> (a,St) }

runSvgMonad :: TextEncoder -> SvgMonad a -> (a,St)
runSvgMonad enc mf = getSvgMonad mf enc st_zero


instance Functor SvgMonad where
  fmap f mf = SvgMonad $ \r s -> let (a,s') = getSvgMonad mf r s 
                                 in (f a, s') 

instance Applicative SvgMonad where
  pure a    = SvgMonad $ \_ s -> (a,s)
  mf <*> ma = SvgMonad $ \r s -> let (f,s1) = getSvgMonad mf r s
                                     (a,s2) = getSvgMonad ma r s1
                                 in (f a, s2)

instance Monad SvgMonad where
  return a  = SvgMonad $ \_ s -> (a,s)
  m >>= k   = SvgMonad $ \r s -> let (a,s1) = getSvgMonad m r s
                                 in (getSvgMonad . k) a r s1


get :: SvgMonad St
get = SvgMonad $ \_ s -> (s,s)

set :: St -> SvgMonad ()
set s = SvgMonad $ \_ _ -> ((),s)

sets_ :: (St -> St) -> SvgMonad ()
sets_ f = SvgMonad $ \_ s -> ((), f s)


ask :: SvgMonad TextEncoder
ask = SvgMonad $ \r s -> (r,s)

asks :: (TextEncoder -> a) -> SvgMonad a
asks f = SvgMonad $ \r s -> (f r,s)


-- | Run the SVG monad.
execSvgMonad :: TextEncoder -> SvgMonad a -> a
execSvgMonad enc mf = fst $ runSvgMonad enc mf


-- | Get the current clip label.
currentClipLabel :: SvgMonad String
currentClipLabel = get >>= return . clipname . clipCount

-- | Generate a new clip label.
newClipLabel :: SvgMonad String
newClipLabel = do 
  i <- (get >>= return . clipCount)
  sets_ (\s -> s { clipCount=i+1 })
  return $ clipname i


clipname :: Int -> String
clipname = ("clip" ++) . show


--------------------------------------------------------------------------------
-- Helpers for XML.Light and /data in strings/.

-- | Helper for XML.Light
unqualAttr :: String -> String -> Attr
unqualAttr name val = Attr (unqual name) val


--------------------------------------------------------------------------------
-- SVG helpers

type SvgPath = [String]


-- | @ \<?xml version=\"1.0\" encoding=\"...\"?\> @
--
xmlVersion :: String -> CData
xmlVersion s = CData CDataRaw 
                     ("<?xml version=\"1.0\" encoding=\"" ++ s ++ "\"?>")
                     (Just 1)

-- |
-- > <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"           
-- >     "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd" > 
--
svgDocType :: CData
svgDocType = CData CDataRaw (line1 ++ "\n" ++ line2) (Just 1)
  where
    line1 = "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
    line2 = "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"

-- | 
-- > <g> ... </g>
--
-- Wumpus uses the g element (group) to achieve nesting. 
gElement :: [Attr] -> [Element] -> Element
gElement xs ys = unode "g" (xs,ys)

-- |
-- > <svg xmlns="http://www.w3.org/2000/svg" version="1.1">
-- > ...
-- > </svg>
--
svgElement :: [Element] -> Element
svgElement xs = unode "svg" ([xmlns,version],xs)
  where
    xmlns   = unqualAttr "xmlns" "http://www.w3.org/2000/svg"
    version = unqualAttr "version" "1.1"  


--------------------------------------------------------------------------------



-- |
-- > <circle/>
--
element_circle :: Element
element_circle = unode "circle" ()

-- |
-- > <ellipse/>
--
element_ellipse :: Element
element_ellipse = unode "ellipse" ()



-- | @ x=\"...\" @
attr_x :: PSUnit u => u -> Attr
attr_x = unqualAttr "x" . dtrunc

-- | @ y=\"...\" @
attr_y :: PSUnit u => u -> Attr
attr_y = unqualAttr "y" . dtrunc

-- | @ r=\"...\" @
attr_r :: PSUnit u => u -> Attr
attr_r = unqualAttr "r" . dtrunc


-- | @ rx=\"...\" @
attr_rx :: PSUnit u => u -> Attr
attr_rx = unqualAttr "rx" . dtrunc

-- | @ ry=\"...\" @
attr_ry :: PSUnit u => u -> Attr
attr_ry = unqualAttr "ry" . dtrunc

-- | @ cx=\"...\" @
attr_cx :: PSUnit u => u -> Attr
attr_cx = unqualAttr "cx" . dtrunc

-- | @ cy=\"...\" @
attr_cy :: PSUnit u => u -> Attr
attr_cy = unqualAttr "cy" . dtrunc




-- |
-- > <path d="..." />
--
-- Note the argument to this function is an attribute rather
-- than content. We have no use for empty paths.
element_path :: SvgPath -> Element
element_path = unode "path" . attr_d

-- |
-- > <clipPath>
-- > ...
-- > </clipPath>
--
element_clippath :: SvgPath -> Element
element_clippath = unode "clipPath" . element_path

-- |
-- > <text>...</text>
--
element_text :: Node t => t -> Element
element_text = unode "text" 

-- |
-- > <text>...</text>
--
element_tspan :: String -> Element
element_tspan = unode "tspan" . content_text


-- | Render the string as 'CDataText' - see XML.Light.
content_text :: String -> Content
content_text str = Text $ CData CDataRaw str Nothing


-- | @ font-family=\"...\" @
attr_font_family :: String -> Attr
attr_font_family = unqualAttr "font-family" 

-- | @ font-size=\"...\" @
attr_font_size :: Int -> Attr
attr_font_size = unqualAttr "font-size" . show

-- | @ font-weight=\"...\" @
attr_font_weight :: String -> Attr
attr_font_weight = unqualAttr "font-weight"

-- | @ font-style=\"...\" @
attr_font_style :: String -> Attr
attr_font_style = unqualAttr "font-style"


-- | @ id=\"...\" @
attr_id :: String -> Attr
attr_id = unqualAttr "id" 

-- | @ d="..." @
attr_d :: SvgPath -> Attr
attr_d = unqualAttr "d" . hsep

-- | @ fill=\"rgb(..., ..., ...)\" @
attr_fill :: PSColour c => c -> Attr
attr_fill = unqualAttr "fill" . val_colour

-- | @ fill=\"none\" @
attr_fill_none :: Attr
attr_fill_none = unqualAttr "fill" "none"

-- | @ stroke=\"rgb(..., ..., ...)\" @
attr_stroke :: PSColour c => c -> Attr
attr_stroke = unqualAttr "stroke" . val_colour

-- | @ stroke=\"none\" @
attr_stroke_none :: Attr
attr_stroke_none = unqualAttr "stroke" "none"

-- | @ stroke-width=\"...\" @
attr_stroke_width :: PSUnit u => u -> Attr
attr_stroke_width = unqualAttr "stroke-width" . dtrunc


-- | @ stroke-miterlimit=\"...\" @
attr_stroke_miterlimit :: PSUnit u => u -> Attr
attr_stroke_miterlimit = unqualAttr "stroke-miterlimit" . dtrunc

-- | @ stroke-linejoin=\"...\" @
attr_stroke_linejoin :: LineJoin -> Attr
attr_stroke_linejoin JoinMiter = unqualAttr "stroke-linejoin" "miter"
attr_stroke_linejoin JoinRound = unqualAttr "stroke-linejoin" "round"
attr_stroke_linejoin JoinBevel = unqualAttr "stroke-linejoin" "bevel"



attr_stroke_linecap :: LineCap -> Attr
attr_stroke_linecap CapButt   = unqualAttr "stroke-linecap" "butt"
attr_stroke_linecap CapRound  = unqualAttr "stroke-linecap" "round"
attr_stroke_linecap CapSquare = unqualAttr "stroke-linecap" "square"


-- | @ stroke-dasharray=\"...\" @
attr_stroke_dasharray :: [Int] -> Attr
attr_stroke_dasharray = unqualAttr "stroke-dasharray" . commasep . map show

-- | @ stroke-dasharray=\"none\" @
attr_stroke_dasharray_none :: Attr
attr_stroke_dasharray_none = unqualAttr "stroke-dasharray" "none"

-- | @ stroke-dashoffset=\"...\" @
attr_stroke_dashoffset :: Int -> Attr
attr_stroke_dashoffset = unqualAttr "stroke-dashoffset" . show

-- | @ color=\"rgb(..., ..., ...)\" @
--
-- Gray or HSB values will be converted to and rendered as RGB.
attr_color :: PSColour c => c -> Attr
attr_color = unqualAttr "color" . val_colour

-- | @ clip-path=\"url(#...)\" @
attr_clippath :: String -> Attr
attr_clippath = unqualAttr "clip-path" . val_url

-- | @ transform="..." @
attr_transform :: String -> Attr
attr_transform = unqualAttr "transform"

-- | @ matrix(..., ..., ..., ..., ..., ...) @
val_matrix :: PSUnit u => u -> u -> u -> u -> u -> u -> String
val_matrix a b c d e f = "matrix" ++ tupled (map dtrunc [a,b,c,d,e,f])



-- | @ rgb(..., ..., ...) @
-- 
-- HSB and gray scale are translated to RGB values.
val_colour :: PSColour c => c -> String
val_colour = val_rgb . psColour


-- | @ rgb(..., ..., ...) @
val_rgb :: RGB3 Double -> String
val_rgb (RGB3 r g b) = "rgb" ++ show (ramp255 r,ramp255 g,ramp255 b)


-- | @ url(#...) @
val_url :: String -> String
val_url s = "url" ++ parens ('#':s)

-- | @ translate(..., ...) @
val_translate :: PSUnit u => u -> u -> String
val_translate x y = "translate" ++ tupled (map dtrunc [x,y])
  
-- | @ M ... ... @
--
-- c.f. PostScript's @moveto@.
path_m :: PSUnit u => u -> u -> String
path_m x y  = hsep $ "M" : map dtrunc [x,y]

-- | @ L ... ... @
--
-- c.f. PostScript's @lineto@.
path_l :: PSUnit u => u -> u -> String
path_l x y  = hsep $ "L" : map dtrunc [x,y]

-- | @ S ... ... ... ... ... ... @
-- 
-- c.f. PostScript's @curveto@.
path_c :: PSUnit u => u -> u -> u -> u -> u -> u -> String
path_c x1 y1 x2 y2 x3 y3 =  hsep $ "C" : map dtrunc [x1,y1,x2,y2,x3,y3]


