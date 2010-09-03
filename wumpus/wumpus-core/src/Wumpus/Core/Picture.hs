{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Picture
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Construction of pictures, paths and text labels.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Picture
  ( 

  -- * Construction
    frame
  , multi
  , path
  , lineTo
  , curveTo
  , vertexPath
  , curvedPath
  , xlinkhref

  -- * Constructing primitives
  , Stroke(..)
  , zostroke
  , zcstroke
  , Fill(..)
  , zfill
  , Bordered(..)
  , clip

  , TextLabel(..)
  , ztextlabel

  , Ellipse(..)
  , zellipse
  , BorderedEllipse(..)
  


  -- * Operations
  , extendBoundary  

  -- * Picture composition
  , picOver
  , picMoveBy
  , picBeside

  -- * Illustrating pictures and primitives
  , printPicture
  , illustrateBounds
  , illustrateBoundsPrim
  , illustrateControlPoints

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.FormatCombinators
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.OneList
import Wumpus.Core.PictureInternal
import Wumpus.Core.PtSize
import Wumpus.Core.TextInternal
import Wumpus.Core.Utils

import Data.AffineSpace                         -- package: vector-space
import Data.Semigroup                           -- package: algebra




--------------------------------------------------------------------------------
-- Construction


-- | Lift a list of primitives to a composite picture.
--
-- The order of the list maps to the zorder - the front of the
-- list is drawn at the top.
--
-- This function throws an error when supplied the empty list.
--
frame :: (Real u, Floating u, FromPtSize u) => [Primitive u] -> Picture u
frame []     = error "Wumpus.Core.Picture.frame - empty list"
frame (p:ps) = let (bb,ones) = step p ps in Leaf (bb,[]) ones 
  where
    step a []     = (boundary a, one a)
    step a (x:xs) = let (bb', rest) = step x xs
                    in ( boundary a `append` bb', cons a rest )



-- | Place multiple pictures within the standard affine frame.
--
-- This function throws an error when supplied the empty list.
--
multi :: (Fractional u, Ord u) => [Picture u] -> Picture u
multi []      = error "Wumpus.Core.Picture.multi - empty list"
multi (p:ps)  = let (bb,ones) = step p ps in Picture (bb,[]) ones 
  where
    step a []     = (boundary a, one a)
    step a (x:xs) = let (bb', rest) = step x xs
                    in ( boundary a `append` bb', cons a rest )



-- | Create a Path from a start point and a list of PathSegments.
--
path :: Point2 u -> [PrimPathSegment u] -> PrimPath u
path = PrimPath 

-- | Create a straight-line PathSegment.
--
lineTo :: Point2 u -> PrimPathSegment u
lineTo = PLineTo

-- | Create a curved PathSegment.
--
curveTo :: Point2 u -> Point2 u -> Point2 u -> PrimPathSegment u
curveTo = PCurveTo


-- | Convert the list of vertices to a path of straight line 
-- segments.
--
vertexPath :: [Point2 u] -> PrimPath u
vertexPath []     = error "Picture.vertexPath - empty point list"
vertexPath (x:xs) = PrimPath x (map PLineTo xs)



-- | Convert a list of vertices to a path of curve segments.
-- The first point in the list makes the start point, each curve 
-- segment thereafter takes 3 points. /Spare/ points at the end 
-- are discarded. 
--
curvedPath :: [Point2 u] -> PrimPath u
curvedPath []     = error "Picture.curvedPath - empty point list"
curvedPath (x:xs) = PrimPath x (step xs) 
  where
    step (a:b:c:ys) = PCurveTo a b c : step ys 
    step _          = []


xlinkhref :: String -> XLink
xlinkhref = XLinkHRef


--------------------------------------------------------------------------------
-- 

-- Design issue.
--
-- The overloading style below - patterned after Iavor S. 
-- Diatchki\'s XML-Light - is (probably) not as valuable as first 
-- anticipated. 
--
-- Wumpus-Basic takes graphic styles from a reader monad, so it 
-- \'always\' calls the overloaded operations with the same 
-- argument types and therefore doesn\'t need overloading.
--
-- It\'s perhaps unlikely that any other software would want to 
-- use Wumpus-Core directly, if it did then it is quite possible 
-- the exact argument types would again be uniform and no
-- shorthand via overloading would be necessary.
--
--


--------------------------------------------------------------------------------
-- Take Paths to Primitives

-- *** Stroke

ostrokePath :: Num u 
            => RGBi -> StrokeAttr -> XLink -> PrimPath u -> Primitive u
ostrokePath rgb sa xlink p = PPath (OStroke sa rgb) xlink p

cstrokePath :: Num u 
            => RGBi -> StrokeAttr -> XLink -> PrimPath u -> Primitive u
cstrokePath rgb sa xlink p = PPath (CStroke sa rgb) xlink p

-- | Create a open, stroked path (@ostroke@) or a closed, stroked
-- path (@cstroke@).
--
-- @ostroke@ and @cstroke@ are overloaded to make attributing 
-- the path more convenient.
-- 
class Stroke t where
  ostroke :: Num u => t -> PrimPath u -> Primitive u
  cstroke :: Num u => t -> PrimPath u -> Primitive u

instance Stroke () where
  ostroke () = ostrokePath black zeroSA NoLink
  cstroke () = cstrokePath black zeroSA NoLink

instance Stroke RGBi where
  ostroke rgb = ostrokePath rgb zeroSA NoLink
  cstroke rgb = cstrokePath rgb zeroSA NoLink

instance Stroke StrokeAttr where
  ostroke sa = ostrokePath black sa NoLink
  cstroke sa = cstrokePath black sa NoLink

instance Stroke XLink where
  ostroke xlink = ostrokePath black zeroSA xlink
  cstroke xlink = cstrokePath black zeroSA xlink


instance Stroke (RGBi,StrokeAttr) where
  ostroke (rgb,sa) = ostrokePath rgb sa NoLink
  cstroke (rgb,sa) = cstrokePath rgb sa NoLink


instance Stroke (RGBi,XLink) where
  ostroke (rgb,xlink) = ostrokePath rgb zeroSA xlink
  cstroke (rgb,xlink) = cstrokePath rgb zeroSA xlink

instance Stroke (StrokeAttr,XLink) where
  ostroke (sa,xlink) = ostrokePath black sa xlink
  cstroke (sa,xlink) = cstrokePath black sa xlink


instance Stroke (RGBi,StrokeAttr,XLink) where
  ostroke (rgb,sa,xlink) = ostrokePath rgb sa xlink
  cstroke (rgb,sa,xlink) = cstrokePath rgb sa xlink



-- | Create an open stoke coloured black.
--
zostroke :: Num u => PrimPath u -> Primitive u
zostroke = ostrokePath black zeroSA NoLink
 
-- | Create a closed stroke coloured black.
--
zcstroke :: Num u => PrimPath u -> Primitive u
zcstroke = cstrokePath black zeroSA NoLink


-- *** Fill

fillPath :: Num u => RGBi -> XLink -> PrimPath u -> Primitive u
fillPath rgb xlink p = PPath (CFill rgb) xlink p

-- | Create a filled path (@fill@). Fills only have one 
-- property - colour. But there are various representations of 
-- colour.
--
-- @ fill () @ will fill with the default colour - black.
-- 
class Fill t where
  fill :: Num u => t -> PrimPath u -> Primitive u
 

instance Fill ()                where fill ()    = fillPath black NoLink
instance Fill RGBi              where fill rgb   = fillPath rgb   NoLink
instance Fill XLink             where fill xlink = fillPath black xlink

instance Fill (RGBi,XLink) where
  fill (rgb,xlink) = fillPath rgb xlink


-- | Create a filled path coloured black. 
zfill :: Num u => PrimPath u -> Primitive u
zfill = fillPath black NoLink


--------------------------------------------------------------------------------
-- Bordered (closed) paths


-- | fill colour * stroke attrs * stroke_colour * ...
--
borderedPath :: Num u 
            => RGBi -> StrokeAttr -> RGBi -> XLink -> PrimPath u -> Primitive u
borderedPath frgb sa srgb xlink p = PPath (CFillStroke frgb sa srgb) xlink p


-- | Create a closed path - that is filled and stroked (the fill
-- is below in the zorder).
--
-- Fill colour must always be supplied, stroke colour and stroke 
-- attributes are optional. The default stroke colour is black.
--
-- 
class Bordered t where
  bordered :: Num u => RGBi -> t -> PrimPath u -> Primitive u

instance Bordered () where 
  bordered frgb () = borderedPath frgb zeroSA black NoLink

instance Bordered RGBi where 
  bordered frgb srgb = borderedPath frgb zeroSA srgb NoLink

instance Bordered StrokeAttr where
  bordered frgb sa = borderedPath frgb sa black NoLink

instance Bordered XLink where 
  bordered frgb xlink = borderedPath frgb zeroSA black xlink

instance Bordered (RGBi,StrokeAttr) where
  bordered frgb (srgb,sa) = borderedPath frgb sa srgb NoLink

instance Bordered (RGBi,XLink) where
  bordered frgb (srgb,xlink) = borderedPath frgb zeroSA srgb xlink

instance Bordered (StrokeAttr,XLink) where
  bordered frgb (sa,xlink) = borderedPath frgb sa black xlink

instance Bordered (RGBi,StrokeAttr,XLink) where
  bordered frgb (srgb,sa,xlink) = borderedPath frgb sa srgb xlink



--------------------------------------------------------------------------------
-- Clipping 

-- | Clip a picture with respect to the supplied path.
--
clip :: (Num u, Ord u) => PrimPath u -> Picture u -> Picture u
clip cp p = Clip (pathBoundary cp, []) cp p

--------------------------------------------------------------------------------
-- Labels to primitive

mkTextLabel :: Num u 
            => RGBi -> FontAttr -> XLink -> String -> Point2 u -> Primitive u
mkTextLabel rgb attr xlink txt pt = PLabel (LabelProps rgb attr) xlink lbl 
  where
    lbl = PrimLabel pt (lexLabel txt) identityCTM

-- | Constant for the default font, which is @Courier@ (aliased 
-- to @Courier New@ for SVG) at 14 point.
--
--
wumpus_default_font :: FontAttr
wumpus_default_font = FontAttr 14 face 
  where
    face = FontFace { font_name         = "Courier"
                    , svg_font_family   = "Courier New"
                    , svg_font_style    = SVG_REGULAR
                    }


-- | Create a text label. The string should not contain newline
-- or tab characters. Use 'multilabel' to create text with 
-- multiple lines.
-- 
-- @textlabel@ is overloaded to make attributing the label more 
-- convenient.
--
-- Unless a 'FontAttr' is specified, the label will use 14pt 
-- Courier.
--
-- The supplied point is is the bottom left corner.
--
class TextLabel t where 
  textlabel :: Num u => t -> String -> Point2 u -> Primitive u


instance TextLabel () where 
    textlabel () = mkTextLabel black wumpus_default_font NoLink

instance TextLabel RGBi where
  textlabel rgb = mkTextLabel rgb wumpus_default_font NoLink

instance TextLabel FontAttr where
  textlabel a = mkTextLabel black a NoLink

instance TextLabel XLink where
    textlabel xlink = mkTextLabel black wumpus_default_font xlink


instance TextLabel (RGBi,FontAttr) where
  textlabel (rgb,a) = mkTextLabel rgb a NoLink

instance TextLabel (RGBi,XLink) where
  textlabel (rgb,xlink) = mkTextLabel rgb wumpus_default_font xlink

instance TextLabel (FontAttr,XLink) where
  textlabel (a,xlink) = mkTextLabel black a xlink

instance TextLabel (RGBi,FontAttr,XLink) where
  textlabel (rgb,a,xlink) = mkTextLabel rgb a xlink

-- | Create a label where the font is @Courier@, text size is 14pt
-- and colour is black.
--
ztextlabel :: Num u => String -> Point2 u -> Primitive u
ztextlabel = mkTextLabel black wumpus_default_font NoLink


--------------------------------------------------------------------------------

mkEllipse :: Num u 
          => EllipseProps -> XLink -> u -> u -> Point2 u -> Primitive u
mkEllipse props xlink hw hh pt = 
    PEllipse props xlink (PrimEllipse pt hw hh identityCTM)


ellipseDefault :: EllipseProps
ellipseDefault = EFill black


-- | Create an ellipse, the ellipse will be filled unless the 
-- supplied attributes /imply/ a stroked ellipse, e.g.:
--
-- > ellipse (LineWidth 4) zeroPt 40 40 
--
-- Note - within Wumpus, ellipses are considered an unfortunate
-- but useful /optimization/. Drawing good cicles with Beziers 
-- needs at least eight curves, but drawing them with 
-- PostScript\'s @arc@ command is a single operation.  For 
-- drawings with many dots (e.g. scatter plots) it seems sensible
-- to employ this optimaztion.
--
-- A deficiency of Wumpus\'s ellipse is that (non-uniformly)
-- scaling a stroked ellipse also (non-uniformly) scales the pen 
-- it is drawn with. Where the ellipse is wider, the pen stroke 
-- will be wider too. 
--
class Ellipse t where
  ellipse :: Num u => t -> u -> u -> Point2 u -> Primitive u


instance Ellipse ()             where ellipse () = zellipse

instance Ellipse RGBi where 
  ellipse rgb = mkEllipse (EFill rgb) NoLink

instance Ellipse StrokeAttr where
  ellipse sa = mkEllipse (EStroke sa black) NoLink

instance Ellipse XLink where 
  ellipse xlink = mkEllipse (EFill black) xlink

instance Ellipse (RGBi,StrokeAttr) where
  ellipse (rgb,sa) = mkEllipse (EStroke sa rgb) NoLink

instance Ellipse (RGBi,XLink) where
  ellipse (rgb,xlink) = mkEllipse (EFill rgb) xlink

instance Ellipse (StrokeAttr,XLink) where
  ellipse (sa,xlink) = mkEllipse (EStroke sa black) xlink


instance Ellipse (RGBi,StrokeAttr,XLink) where
  ellipse (rgb,sa,xlink) = mkEllipse (EStroke sa rgb) xlink

-- | Create a black, filled ellipse. 
zellipse :: Num u => u -> u -> Point2 u -> Primitive u
zellipse hw hh pt = mkEllipse ellipseDefault NoLink hw hh pt

--------------------------------------------------------------------------------
-- Bordered (filled and stroked) ellipse

mkBorderedEllipse :: Num u 
          => RGBi -> StrokeAttr -> RGBi -> XLink -> u -> u -> Point2 u 
          -> Primitive u
mkBorderedEllipse frgb sa srgb xlink hw hh pt = 
    PEllipse (EFillStroke frgb sa srgb) xlink (PrimEllipse pt hw hh identityCTM)

class BorderedEllipse t where
  borderedEllipse :: Num u => RGBi -> t -> u -> u -> Point2 u -> Primitive u


instance BorderedEllipse () where 
  borderedEllipse frgb () = mkBorderedEllipse frgb zeroSA black NoLink

instance BorderedEllipse RGBi where 
  borderedEllipse frgb srgb = mkBorderedEllipse frgb zeroSA srgb NoLink

instance BorderedEllipse StrokeAttr where
  borderedEllipse frgb sa = mkBorderedEllipse frgb sa black NoLink

instance BorderedEllipse XLink where 
  borderedEllipse frgb xlink = mkBorderedEllipse frgb zeroSA black xlink

instance BorderedEllipse (RGBi,StrokeAttr) where
  borderedEllipse frgb (srgb,sa) = mkBorderedEllipse frgb sa srgb NoLink

instance BorderedEllipse (RGBi,XLink) where
  borderedEllipse frgb (srgb,xlink) = mkBorderedEllipse frgb zeroSA srgb xlink

instance BorderedEllipse (StrokeAttr,XLink) where
  borderedEllipse frgb (sa,xlink) = mkBorderedEllipse frgb sa black xlink

instance BorderedEllipse (RGBi,StrokeAttr,XLink) where
  borderedEllipse frgb (srgb,sa,xlink) = mkBorderedEllipse frgb sa srgb xlink



--------------------------------------------------------------------------------
-- Operations

-- | Extend the bounding box of a picture. 
--
-- The bounding box is both horizontal directions by @x@ and 
-- both vertical directions by @y@. @x@ and @y@ must be positive
-- This function cannot be used to shrink a boundary.
--
extendBoundary :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
extendBoundary x y = mapLocale (\(bb,xs) -> (extBB (posve x) (posve y) bb, xs)) 
  where
    extBB x' y' (BBox (P2 x0 y0) (P2 x1 y1)) = BBox pt1 pt2 where 
        pt1 = P2 (x0-x') (y0-y')
        pt2 = P2 (x1+x') (y1+y')
    
    posve n | n < 0     = 0
            | otherwise = n 

--------------------------------------------------------------------------------
-- Minimal support for Picture composition

infixr 6 `picBeside`, `picOver`

-- | 'picOver' : @ picture -> picture -> picture @
--
-- Draw the first picture on top of the second picture - 
-- neither picture will be moved.
--
picOver :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `picOver` b = Picture (bb,[]) (cons a $ one b)
  where
    bb = boundary a `append` boundary b

-- | 'picMoveBy' : @ picture -> vector -> picture @
-- 
--  Move a picture by the supplied vector. 
--
picMoveBy :: (Num u, Ord u) => Picture u -> Vec2 u -> Picture u
p `picMoveBy` (V2 dx dy) = translate dx dy p 

-- | 'picBeside' : @ picture -> picture -> picture @
--
-- Move the second picture to sit at the right side of the
-- first picture
--
picBeside :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `picBeside` b = a `picOver` (b `picMoveBy` v) 
  where 
    (P2 x1 _) = ur_corner $ boundary a
    (P2 x2 _) = ll_corner $ boundary b 
    v         = hvec $ x1 - x2 

--------------------------------------------------------------------------------
-- Illustrating pictures and primitives

-- | Print the syntax tree of a Picture to the console.
--
printPicture :: (Num u, PSUnit u) => Picture u -> IO ()
printPicture pic = putStrLn (show $ format pic) >> putStrLn []


-- | 'illustrateBounds' : @ colour -> picture -> picture @
-- 
-- Draw the picture on top of an image of its bounding box.
-- The bounding box image will be drawn in the supplied colour.
--
illustrateBounds :: (Real u, Floating u, FromPtSize u) 
                 => RGBi -> Picture u -> Picture u
illustrateBounds rgb p = p `picOver` (frame $ boundsPrims rgb p) 


-- | 'illustrateBoundsPrim' : @ colour -> primitive -> picture @
-- 
-- Draw the primitive on top of an image of its bounding box.
-- The bounding box image will be drawn in the supplied colour.
--
-- The result will be lifted from Primitive to Picture.
-- 
illustrateBoundsPrim :: (Real u, Floating u, FromPtSize u) 
                     => RGBi -> Primitive u -> Picture u
illustrateBoundsPrim rgb p = frame (p : boundsPrims rgb p)



-- | Draw a the rectangle of a bounding box, plus cross lines
-- joining the corners.
--
boundsPrims :: (Num u, Ord u, Boundary t, u ~ DUnit t) 
            => RGBi -> t -> [Primitive u]
boundsPrims rgb a = [ bbox_rect, bl_to_tr, br_to_tl ]
  where
    (bl,br,tr,tl) = boundaryCorners $ boundary a
    bbox_rect     = cstroke (rgb, line_attr) $ vertexPath [bl,br,tr,tl]
    bl_to_tr      = ostroke (rgb, line_attr) $ vertexPath [bl,tr]
    br_to_tl      = ostroke (rgb, line_attr) $ vertexPath [br,tl]

    line_attr     = zeroSA { line_cap     = CapRound
                           , dash_pattern = Dash 0 [(1,2)] }


-- | Generate the control points illustrating the Bezier 
-- curves within a picture.
-- 
-- This has no effect on TextLabels.
-- 
-- Pseudo control points are generated for ellipses, 
-- although strictly speaking ellipses do not use Bezier
-- curves - they are implemented with PostScript\'s 
-- @arc@ command.  
--
illustrateControlPoints :: (Real u, Floating u, FromPtSize u)
                        => RGBi -> Primitive u -> Picture u
illustrateControlPoints rgb prim = step prim
  where
    step (PEllipse _ _ e) = frame (prim : ellipseCtrlLines rgb e)
    step (PPath    _ _ p) = frame (prim : pathCtrlLines rgb p)
    step _                = frame [prim]

-- Genrate lines illustrating the control points of curves on 
-- a Path.
--
-- Two lines are generated for a Bezier curve:
-- start-point to control-point1; control-point2 to end-point
--
-- Nothing is generated for a straight line.
--
pathCtrlLines :: (Num u, Ord u) => RGBi -> PrimPath u -> [Primitive u]
pathCtrlLines rgb (PrimPath start ss) = step start ss
  where 
    -- trail the current end point through the recursion...
    step _ []                    = []
    step _ (PLineTo e:xs)        = step e xs
    step s (PCurveTo c1 c2 e:xs) = mkLine s c1 : mkLine c2 e : step e xs 

    mkLine s e                   = ostroke rgb (PrimPath s [lineTo e]) 


-- Generate lines illustrating the control points of an 
-- ellipse:
-- 
-- Two lines for each quadrant: 
-- start-point to control-point1; control-point2 to end-point
--
ellipseCtrlLines :: (Real u, Floating u) 
                 => RGBi -> PrimEllipse u -> [Primitive u]
ellipseCtrlLines rgb pe = start all_points
  where 
    -- list in order: 
    -- [s,cp1,cp2,e, cp1,cp2,e, cp1,cp2,e, cp1,cp2,e]

    all_points           = ellipseControlPoints pe

    start (s:c1:c2:e:xs) = mkLine s c1 : mkLine c2 e : rest e xs
    start _              = []

    rest s (c1:c2:e:xs)  = mkLine s c1 : mkLine c2 e : rest e xs
    rest _ _             = []

    mkLine s e           = ostroke rgb (PrimPath s [lineTo e]) 



-- | Get the control points as a list
-- 
-- There are no duplicates in the list except for the final 
-- /wrap-around/. We take 4 points initially (start,cp1,cp2,end)
-- then (cp1,cp2,end) for the other three quadrants.
--
ellipseControlPoints :: (Floating u, Real u)
                     => PrimEllipse u -> [Point2 u]
ellipseControlPoints (PrimEllipse (P2 x y) hw hh ctm) = 
    map (disp . (new_mtrx *#)) circ
  where
    disp             = (.+^ V2 x y)
    (radius,(dx,dy)) = circleScalingProps hw hh
    new_mtrx         = matrixRepCTM $ scaleCTM dx dy ctm
    circ             = bezierCircle 1 radius (P2 0 0)

    -- subdivide the bezierCircle with 1 to get two
    -- control points per quadrant.    


--
-- I don't know how to calculate bezier arcs (and thus control
-- points) for an ellipse but I know how to do it for a circle...
--
-- So a make a circle with the largest of half-width and 
-- half-height then apply a scale to the points
-- 
circleScalingProps  :: (Fractional u, Ord u) => u -> u -> (u,(u,u))
circleScalingProps hw hh  = (radius, (dx,dy))
  where
    radius     = max hw hh
    (dx,dy)    = if radius == hw then (1, rescale (0,hw) (0,1) hh)
                                 else (rescale (0,hh) (0,1) hw, 1)

