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
  , askEncodingName
  , askGlyphName
  , escapeCharCode
  , setFrameHeight
  , rescalePoint

  , set 
  , get
  , ask
  , asks

  -- * Build SVG
  , SvgPath

  , HAttr

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
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.TextEncoder
import Wumpus.Core.TextEncodingInternal
import Wumpus.Core.Utils

import Text.XML.Light                           -- package: xml


import Control.Applicative


type HAttr = H Attr


data St u = St 
      { clip_count    :: Int
      , frame_height  :: u 
      }

st_zero :: Num u => St u
st_zero = St { clip_count = 0, frame_height = 0 } 

-- | The SVG monad - which wraps a state monad to generate 
-- fresh names.

newtype SvgMonad u a = SvgMonad { 
          getSvgMonad :: TextEncoder -> St u -> (a, St u) }

runSvgMonad :: Num u => TextEncoder -> SvgMonad u a -> (a, St u)
runSvgMonad enc mf = getSvgMonad mf enc st_zero


instance Functor (SvgMonad u) where
  fmap f mf = SvgMonad $ \r s -> let (a,s') = getSvgMonad mf r s 
                                 in (f a, s') 

instance Applicative (SvgMonad u) where
  pure a    = SvgMonad $ \_ s -> (a,s)
  mf <*> ma = SvgMonad $ \r s -> let (f,s1) = getSvgMonad mf r s
                                     (a,s2) = getSvgMonad ma r s1
                                 in (f a, s2)

instance Monad (SvgMonad u) where
  return a  = SvgMonad $ \_ s -> (a,s)
  m >>= k   = SvgMonad $ \r s -> let (a,s1) = getSvgMonad m r s
                                 in (getSvgMonad . k) a r s1


get :: SvgMonad u (St u)
get = SvgMonad $ \_ s -> (s,s)

set :: St u -> SvgMonad u ()
set s = SvgMonad $ \_ _ -> ((),s)

sets_ :: (St u -> St u) -> SvgMonad u ()
sets_ f = SvgMonad $ \_ s -> ((), f s)


ask :: SvgMonad u TextEncoder
ask = SvgMonad $ \r s -> (r,s)

asks :: (TextEncoder -> a) -> SvgMonad u a
asks f = SvgMonad $ \r s -> (f r,s)


-- | Run the SVG monad.
--
execSvgMonad :: Num u => TextEncoder -> SvgMonad u a -> a
execSvgMonad enc mf = fst $ runSvgMonad enc mf


-- | Get the current clip label.
--
currentClipLabel :: SvgMonad u String
currentClipLabel = get >>= return . clipname . clip_count

-- | Generate a new clip label.
newClipLabel :: SvgMonad u String
newClipLabel = do 
  i <- (get >>= return . clip_count)
  sets_ (\s -> s { clip_count = i+1 })
  return $ clipname i


askEncodingName :: SvgMonad u String
askEncodingName = asks svg_encoding_name

askGlyphName :: String -> SvgMonad u (Either GlyphName GlyphName)
askGlyphName nm = SvgMonad $ \r s -> case lookupByGlyphName nm r of
    Just a -> (Right $ escapeCharCode a, s)
    Nothing -> (Left $ escapeCharCode $ svg_fallback r, s)

escapeCharCode :: CharCode -> String
escapeCharCode i = "&#" ++ show i ++ ";"


clipname :: Int -> String
clipname = ("clip" ++) . show


setFrameHeight :: u -> SvgMonad u ()
setFrameHeight h = SvgMonad $ \_ s -> ((), s { frame_height = h })

rescalePoint :: Num u => Point2 u -> SvgMonad u (Point2 u)
rescalePoint (P2 x y) = SvgMonad $ \_ s -> let h = frame_height s in 
                                           (P2 x (h - y), s) 

--------------------------------------------------------------------------------
-- Helpers for XML.Light and /data in strings/.

-- | Helper for XML.Light
--
unqualAttr :: String -> String -> Attr
unqualAttr name val = Attr (unqual name) val


--------------------------------------------------------------------------------
-- SVG helpers

type SvgPath = String


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


-- Attributes are one element Hughes lists so the can be 
-- composed easily.
--

-- | @ x=\"...\" @
--
attr_x :: PSUnit u => u -> HAttr
attr_x = wrapH . unqualAttr "x" . dtrunc

-- | @ y=\"...\" @
--
attr_y :: PSUnit u => u -> HAttr
attr_y = wrapH . unqualAttr "y" . dtrunc

-- | @ r=\"...\" @
--
attr_r :: PSUnit u => u -> HAttr
attr_r = wrapH . unqualAttr "r" . dtrunc


-- | @ rx=\"...\" @
--
attr_rx :: PSUnit u => u -> HAttr
attr_rx = wrapH . unqualAttr "rx" . dtrunc

-- | @ ry=\"...\" @
--
attr_ry :: PSUnit u => u -> HAttr
attr_ry = wrapH . unqualAttr "ry" . dtrunc

-- | @ cx=\"...\" @
--
attr_cx :: PSUnit u => u -> HAttr
attr_cx = wrapH . unqualAttr "cx" . dtrunc

-- | @ cy=\"...\" @
--
attr_cy :: PSUnit u => u -> HAttr
attr_cy = wrapH . unqualAttr "cy" . dtrunc




-- |
-- > <path d="..." />
--
-- Note the argument to this function is an attribute rather
-- than content. We have no use for empty paths.
--
element_path :: SvgPath -> Element
element_path = unode "path" . toListH . attr_d

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
--
content_text :: String -> Content
content_text str = Text $ CData CDataRaw str Nothing


-- | @ font-family=\"...\" @
--
attr_font_family :: String -> HAttr
attr_font_family = wrapH . unqualAttr "font-family" 

-- | @ font-size=\"...\" @
--
attr_font_size :: Int -> HAttr
attr_font_size = wrapH . unqualAttr "font-size" . show

-- | @ font-weight=\"...\" @
--
attr_font_weight :: String -> HAttr
attr_font_weight = wrapH . unqualAttr "font-weight"

-- | @ font-style=\"...\" @
--
attr_font_style :: String -> HAttr
attr_font_style = wrapH . unqualAttr "font-style"


-- | @ id=\"...\" @
--
attr_id :: String -> HAttr
attr_id = wrapH . unqualAttr "id" 

-- | @ d="..." @
--
attr_d :: SvgPath -> HAttr
attr_d = wrapH . unqualAttr "d" 

-- | @ fill=\"rgb(..., ..., ...)\" @
--
attr_fill :: PSColour c => c -> HAttr
attr_fill = wrapH . unqualAttr "fill" . val_colour

-- | @ fill=\"none\" @
--
attr_fill_none :: HAttr
attr_fill_none = wrapH $ unqualAttr "fill" "none"

-- | @ stroke=\"rgb(..., ..., ...)\" @
attr_stroke :: PSColour c => c -> HAttr
attr_stroke = wrapH . unqualAttr "stroke" . val_colour

-- | @ stroke=\"none\" @
--
attr_stroke_none :: HAttr
attr_stroke_none = wrapH $ unqualAttr "stroke" "none"

-- | @ stroke-width=\"...\" @
--
attr_stroke_width :: PSUnit u => u -> HAttr
attr_stroke_width = wrapH . unqualAttr "stroke-width" . dtrunc


-- | @ stroke-miterlimit=\"...\" @
--
attr_stroke_miterlimit :: PSUnit u => u -> HAttr
attr_stroke_miterlimit = wrapH . unqualAttr "stroke-miterlimit" . dtrunc

-- | @ stroke-linejoin=\"...\" @
--
attr_stroke_linejoin :: LineJoin -> HAttr
attr_stroke_linejoin = wrapH . unqualAttr "stroke-linejoin" . step 
  where
    step JoinMiter = "miter"
    step JoinRound = "round"
    step JoinBevel = "bevel"


-- | @ stroke-linecap=\"...\" @
--
attr_stroke_linecap :: LineCap -> HAttr
attr_stroke_linecap = wrapH . unqualAttr "stroke-linecap" . step
  where
    step CapButt   = "butt"
    step CapRound  = "round"
    step CapSquare = "square"


-- | @ stroke-dasharray=\"...\" @
--
attr_stroke_dasharray :: [Int] -> HAttr
attr_stroke_dasharray = 
    wrapH . unqualAttr "stroke-dasharray" . commasep . map show

-- | @ stroke-dasharray=\"none\" @
--
attr_stroke_dasharray_none :: HAttr
attr_stroke_dasharray_none = wrapH $ unqualAttr "stroke-dasharray" "none"

-- | @ stroke-dashoffset=\"...\" @
--
attr_stroke_dashoffset :: Int -> HAttr
attr_stroke_dashoffset = wrapH . unqualAttr "stroke-dashoffset" . show

-- | @ color=\"rgb(..., ..., ...)\" @
--
-- Gray or HSB values will be converted to and rendered as RGB.
--
attr_color :: PSColour c => c -> HAttr
attr_color = wrapH . unqualAttr "color" . val_colour

-- | @ clip-path=\"url(#...)\" @
--
attr_clippath :: String -> HAttr
attr_clippath = wrapH . unqualAttr "clip-path" . val_url

-- | @ transform="..." @
--
attr_transform :: String -> HAttr
attr_transform = wrapH . unqualAttr "transform"

-- | @ matrix(..., ..., ..., ..., ..., ...) @
--
val_matrix :: PSUnit u => u -> u -> u -> u -> u -> u -> String
val_matrix a b c d e f = "matrix" ++ tupled (map dtrunc [a,b,c,d,e,f])



-- | @ rgb(..., ..., ...) @
-- 
-- HSB and gray scale are translated to RGB values.
--
val_colour :: PSColour c => c -> String
val_colour = val_rgb . psColour


-- | @ rgb(..., ..., ...) @
--
val_rgb :: RGB3 Double -> String
val_rgb (RGB3 r g b) = "rgb" ++ show (ramp255 r,ramp255 g,ramp255 b)


-- | @ url(#...) @
--
val_url :: String -> String
val_url s = "url" ++ parens ('#':s)

-- | @ translate(..., ...) @
--
val_translate :: PSUnit u => u -> u -> String
val_translate x y = "translate" ++ tupled (map dtrunc [x,y])
  


spaceS :: ShowS
spaceS = showChar ' '

dtruncS :: PSUnit u => u -> ShowS
dtruncS = showString . dtrunc

--------------------------------------------------------------------------------
-- Monadic path functions, that translate the supplied points

-- | @ M ... ... @
--
-- c.f. PostScript's @moveto@.
--
path_m :: PSUnit u => Point2 u -> SvgMonad u ShowS
path_m pt = 
    (\(P2 x y) -> showChar 'M' . spaceS . dtruncS x .spaceS . dtruncS y)
      <$> rescalePoint pt

-- | @ L ... ... @
--
-- c.f. PostScript's @lineto@.
--
path_l :: PSUnit u => Point2 u -> SvgMonad u ShowS
path_l pt  = 
    (\(P2 x y) -> showChar 'L' . spaceS . dtruncS x . spaceS . dtruncS y)
      <$> rescalePoint pt

-- | @ S ... ... ... ... ... ... @
-- 
-- c.f. PostScript's @curveto@.
--
path_c :: PSUnit u => Point2 u -> Point2 u -> Point2 u -> SvgMonad u ShowS
path_c p1 p2 p3 =  
    (\(P2 x1 y1) (P2 x2 y2) (P2 x3 y3) -> showChar 'C' 
                 . spaceS . dtruncS x1 . spaceS . dtruncS y1
                 . spaceS . dtruncS x2 . spaceS . dtruncS y2
                 . spaceS . dtruncS x3 . spaceS . dtruncS y3)
      <$> rescalePoint p1 <*> rescalePoint p2 <*> rescalePoint p3

