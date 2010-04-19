{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.OutputSVG
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
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

  , writeSVG_latin1
  
  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.PictureInternal
import Wumpus.Core.SVG
import Wumpus.Core.TextEncoder
import Wumpus.Core.TextEncodingInternal
import Wumpus.Core.TextLatin1
import Wumpus.Core.Utils


import MonadLib hiding ( Label )

import Text.XML.Light


type Clipped    = Bool


coordChange ::  (Num u, Ord u, Scale t, u ~ DUnit t) => t -> t
coordChange = scale 1 (-1)


--------------------------------------------------------------------------------

-- | Output a picture to a SVG file. 
writeSVG :: (Ord u, PSUnit u) => FilePath -> TextEncoder -> Picture u -> IO ()
writeSVG filepath enc pic = 
    writeFile filepath $ unlines $ map ppContent $ svgDraw enc pic 

-- | Version of 'writeSVG' - using Latin1 encoding. 
writeSVG_latin1 :: (Ord u, PSUnit u) => FilePath -> Picture u -> IO ()
writeSVG_latin1 filepath = writeSVG filepath latin1Encoder 



svgDraw :: (Ord u, PSUnit u) => TextEncoder -> Picture u -> [Content]
svgDraw enc pic = runSVG enc $ 
    picture False pic' >>= return . topLevelPic mbvec >>= prefixXmlDecls
  where
    pic'      = coordChange pic
    (_,mbvec) = repositionProperties pic'


prefixXmlDecls :: Element -> SvgM [Content]
prefixXmlDecls e = do 
    enc <- asks svg_encoding_name
    let xmlv = xmlVersion enc
    return $ [Text xmlv, Text svgDocType, Elem e]    

topLevelPic :: PSUnit u => Maybe (Vec2 u) -> Element -> Element
topLevelPic Nothing         p = svgElement [p]
topLevelPic (Just (V2 x y)) p = svgElement [gElement [trans_attr] [p]] 
  where 
    trans_attr = attr_transform $ val_translate x y



picture :: (Ord u, PSUnit u) => Clipped -> Picture u -> SvgM Element
picture _ (PicBlank _)            = return $ gElement [] []
picture c (Single (fr,_) prim)    = do 
    elt <- primitive c prim
    return $ gElement (maybe [] return $ frameChange fr) [elt]

picture c (Picture (fr,_) ones)    = do
    es <- toListWithM (picture c) ones
    return $ gElement (maybe [] return $ frameChange fr) es

picture _ (Clip (fr,_) p a) = do 
   cp <- clipPath p
   e1 <- picture True a
   return $ gElement (maybe [] return $ frameChange fr) [cp,e1]


primitive :: (Ord u, PSUnit u) => Clipped -> Primitive u -> SvgM Element
primitive c (PPath props p)            = clipAttrib c $ path props p
primitive c (PLabel props l)           = clipAttrib c $ label props l
primitive c (PEllipse props mid hw hh) = clipAttrib c $ 
                                                ellipse props mid hw hh



-- All clipping paths are closed.
clipPath :: PSUnit u => Path u -> SvgM Element
clipPath p = do
    name <- newClipLabel
    return $ element_clippath ps `rap` add_attr (attr_id name)
  where
    ps = closePath $ pathInstructions p



clipAttrib :: Clipped -> SvgM Element -> SvgM Element
clipAttrib False melt = melt
clipAttrib True  melt = do 
    s   <- currentClipLabel
    elt <- melt
    return $ add_attr (attr_clippath s) elt


-- None of the remaining translation functions need to be in the
-- SvgM monad.

path :: PSUnit u => PathProps -> Path u -> SvgM Element
path (c,dp) p = 
    return $ element_path ps `rap` add_attrs (fill_a : stroke_a : opts)
  where
    (fill_a,stroke_a,opts) = drawProperties c dp
    ps                     = svgPath dp p 


-- Labels need the coordinate system remapping otherwise
-- the will be printed upside down. Both the start point and 
-- the label itself need transforming.
-- 
-- Also rendering coloured text is convoluted (needing the
-- tspan element).
-- 
label :: (Ord u, PSUnit u) => LabelProps -> Label u -> SvgM Element
label (c,FontAttr _ fam style sz) (Label pt entxt) = do 
     str <- encodedText entxt
     let tspan_elt = element_tspan str `rap` add_attrs [ attr_fill c ]
     return $ element_text tspan_elt `rap` add_attrs text_xs 
                                     `rap` add_attrs (fontStyle style)
  where
    P2 x y    = coordChange pt
    text_xs   = [ attr_x x
                , attr_y y 
                , attr_transform $ val_matrix 1 0 0 (-1) 0 (0::Double)
                , attr_font_family fam
                , attr_font_size sz 
                ]
    
    


encodedText :: EncodedText -> SvgM String 
encodedText entxt = 
    let xs = getEncodedText entxt in mapM textChunk  xs >>= return . concat

-- | Unfortunately we can\'t readily put a comment in the 
-- generated SVG when glyph-name lookup fails. Doing similar in 
-- PostScript is easy because we are emiting /linear/ PostScript 
-- as we go along. For SVG we are building an abstract syntax 
-- tree.
-- 
textChunk :: TextChunk -> SvgM String
textChunk (SText s)  = return s
textChunk (EscInt i) = return $ escapeCharCode i
textChunk (EscStr s) = 
    asks (lookupByGlyphName s) >>= maybe failk (return . escapeCharCode) 
  where
    failk = asks svg_fallback >>= return . escapeCharCode 

escapeCharCode :: CharCode -> String
escapeCharCode i = "&#" ++ show i ++ ";"

 
fontStyle :: SVGFontStyle -> [Attr]
fontStyle SVG_REGULAR      = []
fontStyle SVG_BOLD         = [attr_font_weight "bold"]
fontStyle SVG_ITALIC       = [attr_font_style "italic"]
fontStyle SVG_BOLD_ITALIC  = 
    [attr_font_weight "bold", attr_font_style "italic"]
fontStyle SVG_OBLIQUE      = [attr_font_style "oblique"]
fontStyle SVG_BOLD_OBLIQUE = 
    [attr_font_weight "bold", attr_font_style "oblique"]

-- If w==h the draw the ellipse as a circle

ellipse :: PSUnit u => EllipseProps -> Point2 u -> u -> u -> SvgM Element
ellipse (c,dp) (P2 x y) w h 
    | w == h    = return $ element_circle  
                         `rap` add_attrs (circle_attrs  ++ style_attrs)
    | otherwise = return $ element_ellipse 
                         `rap` add_attrs (ellipse_attrs ++ style_attrs)
  where
    circle_attrs  = [attr_cx x, attr_cy y, attr_r w]
    ellipse_attrs = [attr_cx x, attr_cy y, attr_rx w, attr_ry h]
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

drawProperties :: PSColour c => c -> DrawPath -> (Attr, Attr, [Attr])
drawProperties = fn where
  fn c CFill        = (attr_fill c, attr_stroke_none, [])
  fn c (OStroke xs) = (attr_fill_none, attr_stroke c, strokeAttributes xs)
  fn c (CStroke xs) = (attr_fill_none, attr_stroke c, strokeAttributes xs)

drawEllipse :: PSColour c => c -> DrawEllipse -> (Attr, Attr, [Attr])
drawEllipse = fn where
  fn c EFill        = (attr_fill c, attr_stroke_none, [])
  fn c (EStroke xs) = (attr_fill_none, attr_stroke c, strokeAttributes xs)
 

strokeAttributes :: [StrokeAttr] -> [Attr]
strokeAttributes = foldr fn [] where
  fn (LineWidth a)    = (:) (attr_stroke_width a)
  fn (MiterLimit a)   = (:) (attr_stroke_miterlimit a)
  fn (LineCap lc)     = (:) (attr_stroke_linecap lc)
  fn (LineJoin lj)    = (:) (attr_stroke_linejoin lj)
  fn (DashPattern dp) = dash dp where
    dash Solid       = (:) (attr_stroke_dasharray_none)
    dash (Dash _ []) = (:) (attr_stroke_dasharray_none)
    dash (Dash i xs) = (:) (attr_stroke_dashoffset i) . 
                       (:) (attr_stroke_dasharray $ conv xs)
    conv = foldr (\(x,y) a -> x:y:a) []  


svgPath :: PSUnit u => DrawPath -> Path u -> SvgPath
svgPath (OStroke _) p = pathInstructions p
svgPath _           p = closePath $ pathInstructions p


pathInstructions :: PSUnit u => Path u -> [String]
pathInstructions (Path (P2 x y) xs) = path_m x y : map pathSegment xs

pathSegment :: PSUnit u => PathSegment u -> String
pathSegment (PLine (P2 x1 y1))                        = path_l x1 y1
pathSegment (PCurve (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = 
    path_c x1 y1 x2 y2 x3 y3



frameChange :: PSUnit u => Frame2 u -> Maybe Attr
frameChange fr 
    | standardFrame fr = Nothing
    | otherwise        = Just $ attr_transform $ val_matrix a b c d e f 
  where
    CTM a b c d e f = toCTM fr



closePath :: SvgPath -> SvgPath 
closePath xs = xs ++ ["Z"]