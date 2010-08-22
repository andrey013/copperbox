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

import Wumpus.Core.BoundingBox
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.PictureInternal
import Wumpus.Core.SVG
import Wumpus.Core.TextEncoder
import Wumpus.Core.TextEncodingInternal
import Wumpus.Core.TextLatin1
import Wumpus.Core.Utils

import Text.XML.Light                           -- package: xml

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F


type Clipped    = Bool


dZero :: Double
dZero = 0


-- coordChange ::  (Num u, Scale t, u ~ DUnit t) => t -> t
-- coordChange = scale 1 (-1)

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
svgDraw enc pic = execSvgMonad enc pg_height $ do
    elem1     <- picture False pic
    prefixXmlDecls (topLevelPic mbvec elem1)
  where
    (_,mbvec) = repositionProperties pic
    pg_height = boundaryHeight $ boundary pic

prefixXmlDecls :: Element -> SvgMonad u [Content]
prefixXmlDecls e = do 
    enc <- askEncodingName
    let xmlv = xmlVersion enc
    return $ [Text xmlv, Text svgDocType, Elem e]    

topLevelPic :: PSUnit u => Maybe (Vec2 u) -> Element -> Element
topLevelPic Nothing         p = svgElement [p]
topLevelPic (Just (V2 x y)) p = svgElement [gElement trans_attribs [p]] 
  where 
    trans_attribs = toListH $ attr_transform $ val_translate x y



picture :: (Real u, Floating u, PSUnit u) 
        => Clipped -> Picture u -> SvgMonad u Element
picture _ (PicBlank _)              = return $ gElement [] []

picture c (Leaf (fr,bb,h) ones)     = do 
    setFrameHeight $ frameHeight h
    elts <- F.foldlM (\acc e -> do { a <- primitive c e; return (a:acc) }) [] ones
    fr_attrs <- frameChangeLeaf (boundaryHeight bb) fr
    return $ gElement (toListH fr_attrs) elts

picture c (Picture (fr,bb,_) ones)  = do
    (fr_attrs,dy) <- frameChangeNode (boundaryHeight bb) fr 
    elts <- withExtDelta dy $ F.foldlM (\acc e -> do { a <- picture c e
                                                     ; return (a:acc) }) [] ones
    return $ gElement (toListH fr_attrs) elts
  
picture _ (Clip (fr,bb,_) p a)      = do 
    (fr_attrs,dy) <- frameChangeNode (boundaryHeight bb) fr 
    cp <- clipPath p
    e1 <- withExtDelta dy $ picture True a
    return $ gElement (toListH fr_attrs) [cp,e1]


primitive :: (Real u, Floating u, PSUnit u) 
          => Clipped -> Primitive u -> SvgMonad u Element
primitive c (PPath props p)     = clipAttrib c $ path props p
primitive c (PLabel props l)    = clipAttrib c $ label props l
primitive c (PEllipse props e)  = clipAttrib c $ ellipse props e



-- All clipping paths are closed.
clipPath :: PSUnit u => Path u -> SvgMonad u Element
clipPath p = (\name ps -> element_clippath ps `snoc_attrs` attr_id name)
                <$> newClipLabel <*> closedPath p



clipAttrib :: Clipped -> SvgMonad u Element -> SvgMonad u Element
clipAttrib False melt = melt
clipAttrib True  melt = do 
    s   <- currentClipLabel
    elt <- melt
    return $ elt `snoc_attrs` (attr_clippath s)


-- None of the remaining translation functions need to be in the
-- SvgMonad monad.

path :: PSUnit u => PathProps -> Path u -> SvgMonad u Element
path (c,dp) p = 
    svgPath dp p >>= \ ps -> return $ element_path ps `snoc_attrs` attrs
  where
    attrs = pathAttrs c dp


-- Labels need the coordinate system remapping otherwise
-- the will be printed upside down. Both the start point and 
-- the label itself need transforming.
-- 
-- Also rendering coloured text is convoluted (needing the
-- tspan element).
-- 
--
label :: (Real u, Floating u, PSUnit u) 
      => LabelProps -> Label u -> SvgMonad u Element
label (c,FontAttr sz face) (Label pt entxt ctm) = do 
     str <- encodedText entxt
     coord_attrs <- labelGeom pt ctm
     let tspan_elt = element_tspan str `snoc_attrs` (attr_fill c)
     return $ element_text tspan_elt `snoc_attrs` ( coord_attrs
                                                  . font_desc
                                                  . fontAttrs style )
  where
    style       = svg_font_style  face
    fam         = svg_font_family face
    font_desc   = attr_font_family fam . attr_font_size sz 
                  


encodedText :: EncodedText -> SvgMonad u String 
encodedText entxt = 
    let xs = getEncodedText entxt in mapM textChunk  xs >>= return . concat

-- | Unfortunately we can\'t readily put a comment in the 
-- generated SVG when glyph-name lookup fails. Doing similar in 
-- PostScript is easy because we are emiting /linear/ PostScript 
-- as we go along. For SVG we are building an abstract syntax 
-- tree.
-- 
textChunk :: TextChunk -> SvgMonad u String
textChunk (SText s)  = return s
textChunk (EscInt i) = return $ escapeCharCode i
textChunk (EscStr s) = askGlyphName s >>= either return return



-- If w==h the draw the ellipse as a circle

ellipse :: (Real u, Floating u, PSUnit u)
        => EllipseProps -> PrimEllipse u -> SvgMonad u Element
ellipse (c,dp) (PrimEllipse pt hw hh ctm) 
    | hw == hh  = (\geomf -> element_circle `snoc_attrs` 
                              (circleRadius hw . geomf . style_attrs))
                    <$> ellipseGeom pt ctm 
    | otherwise = (\geomf -> element_ellipse `snoc_attrs` 
                              (ellipseRadius hw hh . geomf . style_attrs))
                    <$> ellipseGeom pt ctm
  where
    style_attrs   = ellipseAttrs c dp


--------------------------------------------------------------------------------
-- 

labelGeom :: (Real u, Floating u, PSUnit u) 
          => Point2 u -> PrimCTM u -> SvgMonad u HAttr
labelGeom pt ctm 
    | ctm == identityCTM = (\(P2 x y) -> attr_x x . attr_y y) 
                              <$> rescalePoint pt
    | otherwise          = (\(P2 x y) -> attr_x dZero . attr_y dZero 
                                                      . mtrxTrafo x y)
                              <$> rescalePoint pt
  where
    mtrxTrafo x y = attr_transform $ valMatrix $ 
                       translationMatrix x y * (invert $ matrixRepCTM ctm)



circleRadius :: PSUnit u => u -> HAttr
circleRadius radius = attr_r radius

ellipseRadius :: PSUnit u => u -> u -> HAttr
ellipseRadius hw hh = attr_rx hw . attr_ry hh

ellipseGeom :: (Real u, Floating u, PSUnit u)
           => Point2 u -> PrimCTM u -> SvgMonad u HAttr
ellipseGeom pt ctm 
    | ctm == identityCTM = (\(P2 x y) -> attr_cx x . attr_cy y )
                              <$> rescalePoint pt
    | otherwise          = (\(P2 x y) -> attr_cx dZero . attr_cy dZero
                                                       . mtrxTrafo x y ) 
                              <$> rescalePoint pt
  where
    mtrxTrafo x y = attr_transform $ valMatrix $ 
                       translMatrixRepCTM x y ctm * svg_reflection_matrix



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

svgPath :: PSUnit u => DrawPath -> Path u -> SvgMonad u String
svgPath (OStroke _) p = liftM toListH $ pathK p id
svgPath _           p = closedPath p


closedPath :: PSUnit u => Path u -> SvgMonad u String
closedPath p = liftM toListH $ pathK p (showString " Z")

pathK :: PSUnit u => Path u -> ShowS -> SvgMonad u ShowS
pathK (Path start_pt xs) end =  (\prefix body -> prefix . body . end)
    <$> path_m start_pt <*> pathSegments xs


pathSegments :: PSUnit u => [PathSegment u] -> SvgMonad u ShowS
pathSegments = F.foldrM mf id
  where
    mf e accf = pathSegment e >>= \f  -> return (showChar ' ' . f . accf)


pathSegment :: PSUnit u => PathSegment u -> SvgMonad u ShowS
pathSegment (PLineTo pt)        = path_l pt
pathSegment (PCurveTo p1 p2 p3) = path_c p1 p2 p3


--------------------------------------------------------------------------------

frameChangeNode :: PSUnit u => u -> Frame2 u -> SvgMonad u (HAttr,u)
frameChangeNode _  fr@(Frame2 (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy))
    | standardFrame fr  = return (id,0)
    | otherwise         = return (mtrx_attr,oy)
  where
    mtrx_attr = attr_transform $ val_matrix e0x e0y e1x e1y ox 0


frameChangeLeaf :: PSUnit u => u -> Frame2 u -> SvgMonad u HAttr
frameChangeLeaf bb_height (Frame2 (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)) =
    (\pg_height inh_delta -> mk $ pg_height - bb_height - inh_delta - oy)
       <$> askPageHeight <*> getInhDelta      
  where
    mk y = attr_transform $ val_matrix e0x e0y e1x e1y ox y




snoc_attrs :: Element -> HAttr -> Element
snoc_attrs e f = (toListH f) `add_attrs` e



valMatrix :: PSUnit u => Matrix3'3 u -> String
valMatrix m33 = val_matrix a b c d x y
  where
    CTM a b c d x y = toCTM m33