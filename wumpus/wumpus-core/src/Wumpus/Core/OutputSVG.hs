{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.OutputSVG
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
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

import Text.XML.Light                           -- package: xml

import qualified Data.Foldable as F


type Clipped    = Bool


dZero :: Double
dZero = 0


coordChange ::  (Num u, Scale t, u ~ DUnit t) => t -> t
coordChange = scale 1 (-1)

svg_reflection_matrix :: Num u => Matrix3'3 u
svg_reflection_matrix = scalingMatrix 1 (-1)

--------------------------------------------------------------------------------

-- | Output a picture to a SVG file. 
--
writeSVG :: (Real u, Floating u, PSUnit u) 
         => FilePath -> TextEncoder -> Picture u -> IO ()
writeSVG filepath enc pic = 
    writeFile filepath $ unlines $ map ppContent $ svgDraw enc pic 

-- | Version of 'writeSVG' - using Latin1 encoding. 
--
writeSVG_latin1 :: (Real u, Floating u, PSUnit u) 
                => FilePath -> Picture u -> IO ()
writeSVG_latin1 filepath = writeSVG filepath latin1Encoder 



svgDraw :: (Real u, Floating u, PSUnit u) 
        => TextEncoder -> Picture u -> [Content]
svgDraw enc pic = execSvgMonad enc $ do
    elem1     <- picture False pic'
    prefixXmlDecls (topLevelPic mbvec elem1)
  where
    pic'      = coordChange pic
    (_,mbvec) = repositionProperties pic'


prefixXmlDecls :: Element -> SvgMonad [Content]
prefixXmlDecls e = do 
    enc <- askEncodingName
    let xmlv = xmlVersion enc
    return $ [Text xmlv, Text svgDocType, Elem e]    

topLevelPic :: PSUnit u => Maybe (Vec2 u) -> Element -> Element
topLevelPic Nothing         p = svgElement [p]
topLevelPic (Just (V2 x y)) p = svgElement [gElement trans_attribs [p]] 
  where 
    trans_attribs = toListH $ attr_transform $ val_translate x y


-- Note - Leaf case reverses the list as it goes to respect the 
-- zorder.
--
picture :: (Real u, Floating u, PSUnit u) 
        => Clipped -> Picture u -> SvgMonad Element
picture _ (PicBlank _)            = return $ gElement [] []
picture c (Leaf (fr,_) _ ones)    = do 
    elts <- F.foldlM (\acc e -> do { a <- primitive c e; return (a:acc) }) [] ones
    return $ gElement (toListH $ frameChange fr) elts

picture c (Picture (fr,_) ones)   = do
    elts <- F.foldlM (\acc e -> do { a <- picture c e; return (a:acc) }) [] ones
    return $ gElement (toListH $ frameChange fr) elts
  
picture _ (Clip (fr,_) p a)       = do 
   cp <- clipPath p
   e1 <- picture True a
   return $ gElement (toListH $ frameChange fr) [cp,e1]


primitive :: (Real u, Floating u, PSUnit u) 
          => Clipped -> Primitive u -> SvgMonad Element
primitive c (PPath props p)     = clipAttrib c $ path props p
primitive c (PLabel props l)    = clipAttrib c $ label props l
primitive c (PEllipse props e)  = clipAttrib c $ ellipse props e



-- All clipping paths are closed.
clipPath :: PSUnit u => Path u -> SvgMonad Element
clipPath p = do
    name <- newClipLabel
    return $ element_clippath ps `snoc_attrs` attr_id name
  where
    ps = closePath $ pathInstructions p



clipAttrib :: Clipped -> SvgMonad Element -> SvgMonad Element
clipAttrib False melt = melt
clipAttrib True  melt = do 
    s   <- currentClipLabel
    elt <- melt
    return $ elt `snoc_attrs` (attr_clippath s)


-- None of the remaining translation functions need to be in the
-- SvgMonad monad.

path :: PSUnit u => PathProps -> Path u -> SvgMonad Element
path (c,dp) p = 
    return $ element_path ps `snoc_attrs` attrs
  where
    attrs = pathAttrs c dp
    ps    = svgPath dp p 


-- Labels need the coordinate system remapping otherwise
-- the will be printed upside down. Both the start point and 
-- the label itself need transforming.
-- 
-- Also rendering coloured text is convoluted (needing the
-- tspan element).
-- 
--
label :: (Real u, Floating u, PSUnit u) 
      => LabelProps -> Label u -> SvgMonad Element
label (c,FontAttr sz face) (Label pt entxt ctm) = do 
     str <- encodedText entxt
     let tspan_elt = element_tspan str `snoc_attrs` (attr_fill c)
     return $ element_text tspan_elt `snoc_attrs` ( coord_attrs
                                                  . font_desc
                                                  . fontAttrs style )
  where
    style       = svg_font_style  face
    fam         = svg_font_family face
    coord_attrs = if ctm == identityCTM then simpleLabelAttrs pt
                                        else transfLabelAttrs pt ctm
    font_desc   = attr_font_family fam . attr_font_size sz 
                  

simpleLabelAttrs :: PSUnit u => Point2 u -> HAttr
simpleLabelAttrs pt = attr_x x . attr_y y . attr_transform mtrx
  where
    P2 x y    = coordChange pt
    mtrx      = val_matrix 1 0 0 (-1) 0 dZero
    
transfLabelAttrs :: (Real u, Floating u, PSUnit u) 
                 => Point2 u -> PrimCTM u -> HAttr
transfLabelAttrs (P2 x y) ctm = 
    attr_x dZero . attr_y dZero . attr_transform vmtrx
  where
    mtrx          = translMatrixRepCTM x y ctm * svg_reflection_matrix
    vmtrx         = valMatrix mtrx


encodedText :: EncodedText -> SvgMonad String 
encodedText entxt = 
    let xs = getEncodedText entxt in mapM textChunk  xs >>= return . concat

-- | Unfortunately we can\'t readily put a comment in the 
-- generated SVG when glyph-name lookup fails. Doing similar in 
-- PostScript is easy because we are emiting /linear/ PostScript 
-- as we go along. For SVG we are building an abstract syntax 
-- tree.
-- 
textChunk :: TextChunk -> SvgMonad String
textChunk (SText s)  = return s
textChunk (EscInt i) = return $ escapeCharCode i
textChunk (EscStr s) = askGlyphName s >>= either return return



-- If w==h the draw the ellipse as a circle

ellipse :: (Real u, Floating u, PSUnit u)
        => EllipseProps -> PrimEllipse u -> SvgMonad Element
ellipse (c,dp) (PrimEllipse pt hw hh ctm) 
    | hw == hh  = return $ element_circle  
                            `snoc_attrs` (circle_attrs  . style_attrs)
    | otherwise = return $ element_ellipse 
                            `snoc_attrs` (ellipse_attrs . style_attrs)
  where
    circle_attrs  = if ctm == identityCTM 
                      then simpleCircleAttrs pt hw
                      else transfCircleAttrs pt hw ctm

    ellipse_attrs = if ctm == identityCTM 
                      then simpleEllipseAttrs pt hw hh 
                      else transfEllipseAttrs pt hw hh ctm

    style_attrs   = ellipseAttrs c dp

simpleCircleAttrs :: PSUnit u => Point2 u -> u -> HAttr
simpleCircleAttrs (P2 x y) radius = attr_cx x . attr_cy y . attr_r radius

simpleEllipseAttrs :: PSUnit u => Point2 u -> u -> u -> HAttr
simpleEllipseAttrs (P2 x y) hw hh = 
    attr_cx x . attr_cy y . attr_rx hw . attr_ry hh


transfCircleAttrs :: (Real u, Floating u, PSUnit u)
                  => Point2 u -> u -> PrimCTM u -> HAttr
transfCircleAttrs (P2 x y) radius ctm = 
    attr_cx dZero . attr_cy dZero . attr_r radius . attr_transform mtrx 
  where
    mtrx  = valMatrix $ translMatrixRepCTM x y ctm * svg_reflection_matrix


transfEllipseAttrs :: (Real u, Floating u, PSUnit u)
                   => Point2 u -> u -> u -> PrimCTM u -> HAttr
transfEllipseAttrs (P2 x y) hw hh ctm = 
    attr_cx dZero . attr_cy dZero . attr_rx hw . attr_ry hh
                  . attr_transform mtrx 
  where
    mtrx  = valMatrix $ translMatrixRepCTM x y ctm * svg_reflection_matrix


-- A rule of thumb seems to be that SVG (at least SVG in Firefox)
-- will try to fill unless told not to. So always label paths
-- with @fill=...@ even if fill is @\"none\"@.
--
-- CFill   ==> stroke="none" fill="..."
-- CStroke ==> stroke="..."  fill="none"
-- OStroke ==> stroke="..."  fill="none"
--

pathAttrs :: PSColour c => c -> DrawPath -> HAttr
pathAttrs c CFill        = attr_fill c . attr_stroke_none
pathAttrs c (OStroke xs) = attr_fill_none . attr_stroke c . strokeAttrs xs
pathAttrs c (CStroke xs) = attr_fill_none . attr_stroke c . strokeAttrs xs

ellipseAttrs :: PSColour c => c -> DrawEllipse -> HAttr
ellipseAttrs c EFill        = attr_fill c . attr_stroke_none
ellipseAttrs c (EStroke xs) = attr_fill_none . attr_stroke c . strokeAttrs xs
 

strokeAttrs :: [StrokeAttr] -> HAttr
strokeAttrs = foldr (\a f -> stroke1 a . f) id

stroke1 :: StrokeAttr -> HAttr
stroke1 (LineWidth a)    = attr_stroke_width a
stroke1 (MiterLimit a)   = attr_stroke_miterlimit a
stroke1 (LineCap lc)     = attr_stroke_linecap lc
stroke1 (LineJoin lj)    = attr_stroke_linejoin lj
stroke1 (DashPattern dp) = dashAttrs dp

dashAttrs :: DashPattern -> HAttr
dashAttrs Solid       = attr_stroke_dasharray_none
dashAttrs (Dash _ []) = attr_stroke_dasharray_none
dashAttrs (Dash i xs) = 
    attr_stroke_dashoffset i . (attr_stroke_dasharray $ conv xs)
  where
    conv = foldr (\(x,y) a -> x:y:a) []  


 
fontAttrs :: SVGFontStyle -> HAttr
fontAttrs SVG_REGULAR      = emptyH
fontAttrs SVG_BOLD         = attr_font_weight "bold"
fontAttrs SVG_ITALIC       = attr_font_style "italic"
fontAttrs SVG_BOLD_ITALIC  = attr_font_weight "bold" . attr_font_style "italic"
fontAttrs SVG_OBLIQUE      = attr_font_style "oblique"
fontAttrs SVG_BOLD_OBLIQUE = attr_font_weight "bold" . attr_font_style "oblique"


--------------------------------------------------------------------------------
-- paths

-- Ah, can do better than this ...

svgPath :: PSUnit u => DrawPath -> Path u -> SvgPath
svgPath (OStroke _) p = pathInstructions p
svgPath _           p = closePath $ pathInstructions p


pathInstructions :: PSUnit u => Path u -> [String]
pathInstructions (Path (P2 x y) xs) = path_m x y : map pathSegment xs

pathSegment :: PSUnit u => PathSegment u -> String
pathSegment (PLineTo (P2 x1 y1))                        = path_l x1 y1
pathSegment (PCurveTo (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = 
    path_c x1 y1 x2 y2 x3 y3

closePath :: SvgPath -> SvgPath 
closePath xs = xs ++ ["Z"]


--------------------------------------------------------------------------------

frameChange :: PSUnit u => Frame2 u -> HAttr
frameChange fr 
    | standardFrame fr = emptyH
    | otherwise        = attr_transform $ val_matrix a b c d e f 
  where
    CTM a b c d e f = toCTM fr




snoc_attrs :: Element -> HAttr -> Element
snoc_attrs e f = (toListH f) `add_attrs` e



valMatrix :: PSUnit u => Matrix3'3 u -> String
valMatrix m33 = val_matrix a b c d x y
  where
    CTM a b c d x y = toCTM m33