{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.Picture
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh picture.
--
--------------------------------------------------------------------------------


module Wumpus.Fresh.Picture
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
  , Ellipse(..)
  , zellipse

  , clip

  , TextLabel(..)
  , ztextlabel


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

import Wumpus.Fresh.AffineTrans
import Wumpus.Fresh.BoundingBox
import Wumpus.Fresh.Colour
import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.GraphicsState
import Wumpus.Fresh.OneList
import Wumpus.Fresh.PictureInternal
import Wumpus.Fresh.PtSize
import Wumpus.Fresh.TextInternal
import Wumpus.Fresh.Utils

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
frame (p:ps) = let (bb,yr,ones) = step p ps in Leaf (bb,[],yr) ones 
  where
    step a []     = (boundary a, primitiveYRange a, one a)
    step a (x:xs) = let (bb', yr', rest) = step x xs
                    in ( boundary a `append` bb'
                       , primitiveYRange a `append` yr'
                       , cons a rest )



-- | Place multiple pictures within the standard affine frame.
--
-- This function throws an error when supplied the empty list.
--
multi :: (Fractional u, Ord u) => [Picture u] -> Picture u
multi []      = error "Wumpus.Core.Picture.multi - empty list"
multi (p:ps)  = let (bb,yr,ones) = step p ps in Picture (bb,[],yr) ones 
  where
    step a []     = (boundary a, yrange a, one a)
    step a (x:xs) = let (bb',yr',rest) = step x xs
                    in ( boundary a `append` bb'
                       , yrange a `append` yr'
                       , cons a rest )



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
-- Take Paths to Primitives

-- *** Stroke

ostrokePath :: Num u 
            => RGB255 -> [StrokeAttr] -> XLink -> PrimPath u -> Primitive u
ostrokePath rgb attrs xlink p = PPath (OStroke attrs rgb) xlink p

cstrokePath :: Num u 
            => RGB255 -> [StrokeAttr] -> XLink -> PrimPath u -> Primitive u
cstrokePath rgb attrs xlink p = PPath (CStroke attrs rgb) xlink p

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
  ostroke () = ostrokePath black [] NoLink
  cstroke () = cstrokePath black [] NoLink

instance Stroke RGB255 where
  ostroke rgb = ostrokePath rgb [] NoLink
  cstroke rgb = cstrokePath rgb [] NoLink

instance Stroke StrokeAttr where
  ostroke x = ostrokePath black [x] NoLink
  cstroke x = cstrokePath black [x] NoLink

instance Stroke [StrokeAttr] where
  ostroke xs = ostrokePath black xs NoLink
  cstroke xs = cstrokePath black xs NoLink

instance Stroke XLink where
  ostroke xlink = ostrokePath black [] xlink
  cstroke xlink = cstrokePath black [] xlink


instance Stroke (RGB255,StrokeAttr) where
  ostroke (rgb,x) = ostrokePath rgb [x] NoLink
  cstroke (rgb,x) = cstrokePath rgb [x] NoLink

instance Stroke (RGB255,[StrokeAttr]) where
  ostroke (rgb,xs) = ostrokePath rgb xs NoLink
  cstroke (rgb,xs) = cstrokePath rgb xs NoLink

instance Stroke (RGB255,XLink) where
  ostroke (rgb,xlink) = ostrokePath rgb [] xlink
  cstroke (rgb,xlink) = cstrokePath rgb [] xlink

instance Stroke (StrokeAttr,XLink) where
  ostroke (x,xlink) = ostrokePath black [x] xlink
  cstroke (x,xlink) = cstrokePath black [x] xlink

instance Stroke ([StrokeAttr],XLink) where
  ostroke (xs,xlink) = ostrokePath black xs xlink
  cstroke (xs,xlink) = cstrokePath black xs xlink

instance Stroke (RGB255,StrokeAttr,XLink) where
  ostroke (rgb,x,xlink) = ostrokePath rgb [x] xlink
  cstroke (rgb,x,xlink) = cstrokePath rgb [x] xlink

instance Stroke (RGB255,[StrokeAttr],XLink) where
  ostroke (rgb,xs,xlink) = ostrokePath rgb xs xlink
  cstroke (rgb,xs,xlink) = cstrokePath rgb xs xlink


-- | Create an open stoke coloured black.
--
zostroke :: Num u => PrimPath u -> Primitive u
zostroke = ostrokePath black [] NoLink
 
-- | Create a closed stroke coloured black.
--
zcstroke :: Num u => PrimPath u -> Primitive u
zcstroke = cstrokePath black [] NoLink


-- *** Fill

fillPath :: Num u => RGB255 -> XLink -> PrimPath u -> Primitive u
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
instance Fill RGB255            where fill rgb   = fillPath rgb   NoLink
instance Fill XLink             where fill xlink = fillPath black xlink

instance Fill (RGB255,XLink) where
  fill (rgb,xlink) = fillPath rgb xlink


-- | Create a filled path coloured black. 
zfill :: Num u => PrimPath u -> Primitive u
zfill = fillPath black NoLink

--------------------------------------------------------------------------------
-- Clipping 

-- | Clip a picture with respect to the supplied path.
--
clip :: (Num u, Ord u) => PrimPath u -> Picture u -> Picture u
clip cp p = Clip (pathBoundary cp, [], yrange p) cp p

--------------------------------------------------------------------------------
-- Labels to primitive

mkTextLabel :: Num u 
            => RGB255 -> FontAttr -> XLink -> String -> Point2 u -> Primitive u
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

instance TextLabel RGB255 where
  textlabel rgb = mkTextLabel rgb wumpus_default_font NoLink

instance TextLabel FontAttr where
  textlabel a = mkTextLabel black a NoLink

instance TextLabel XLink where
    textlabel xlink = mkTextLabel black wumpus_default_font xlink


instance TextLabel (RGB255,FontAttr) where
  textlabel (rgb,a) = mkTextLabel rgb a NoLink

instance TextLabel (RGB255,XLink) where
  textlabel (rgb,xlink) = mkTextLabel rgb wumpus_default_font xlink

instance TextLabel (FontAttr,XLink) where
  textlabel (a,xlink) = mkTextLabel black a xlink

instance TextLabel (RGB255,FontAttr,XLink) where
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
  ellipse :: Fractional u => t -> u -> u -> Point2 u -> Primitive u


instance Ellipse ()             where ellipse () = zellipse

instance Ellipse RGB255 where 
  ellipse rgb = mkEllipse (EFill rgb) NoLink

instance Ellipse StrokeAttr where
  ellipse x = mkEllipse (EStroke [x] black) NoLink

instance Ellipse [StrokeAttr] where
  ellipse xs = mkEllipse (EStroke xs black) NoLink

instance Ellipse XLink where 
  ellipse xlink = mkEllipse (EFill black) xlink

instance Ellipse (RGB255,StrokeAttr) where
  ellipse (rgb,x) = mkEllipse (EStroke [x] rgb) NoLink

instance Ellipse (RGB255,[StrokeAttr]) where
  ellipse (rgb,xs) = mkEllipse (EStroke xs rgb) NoLink

instance Ellipse (RGB255,XLink) where
  ellipse (rgb,xlink) = mkEllipse (EFill rgb) xlink

instance Ellipse (StrokeAttr,XLink) where
  ellipse (x,xlink) = mkEllipse (EStroke [x] black) xlink

instance Ellipse ([StrokeAttr],XLink) where
  ellipse (xs,xlink) = mkEllipse (EStroke xs black) xlink

instance Ellipse (RGB255,[StrokeAttr],XLink) where
  ellipse (rgb,xs,xlink) = mkEllipse (EStroke xs rgb) xlink

-- | Create a black, filled ellipse. 
zellipse :: Num u => u -> u -> Point2 u -> Primitive u
zellipse hw hh pt = mkEllipse ellipseDefault NoLink hw hh pt


--------------------------------------------------------------------------------
-- Minimal support for Picture composition

infixr 6 `picBeside`, `picOver`

-- | 'picOver' : @ picture -> picture -> picture @
--
-- Draw the first picture on top of the second picture - 
-- neither picture will be moved.
--
picOver :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `picOver` b = Picture (bb,[],yr) (cons a $ one b)
  where
    bb = boundary a `append` boundary b
    yr = yrange a   `append` yrange b

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
                 => RGB255 -> Picture u -> Picture u
illustrateBounds rgb p = p `picOver` (frame $ boundsPrims rgb p) 


-- | 'illustrateBoundsPrim' : @ colour -> primitive -> picture @
-- 
-- Draw the primitive on top of an image of its bounding box.
-- The bounding box image will be drawn in the supplied colour.
--
-- The result will be lifted from Primitive to Picture.
-- 
illustrateBoundsPrim :: (Real u, Floating u, FromPtSize u) 
                     => RGB255 -> Primitive u -> Picture u
illustrateBoundsPrim rgb p = frame (boundsPrims rgb p ++ [p])

-- Note - above has to use snoc (++ [p]) to get the picture to
-- draw above the bounding box image.


-- | Draw a the rectangle of a bounding box, plus cross lines
-- joining the corners.
--
boundsPrims :: (Num u, Ord u, Boundary t, u ~ DUnit t) 
            => RGB255 -> t -> [Primitive u]
boundsPrims rgb a = [ bbox_rect, bl_to_tr, br_to_tl ]
  where
    (bl,br,tr,tl) = boundaryCorners $ boundary a
    bbox_rect     = cstroke rgb $ vertexPath [bl,br,tr,tl]
    bl_to_tr      = ostroke rgb $ vertexPath [bl,tr]
    br_to_tl      = ostroke rgb $ vertexPath [br,tl]



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
                        => RGB255 -> Primitive u -> Picture u
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
pathCtrlLines :: (Num u, Ord u) => RGB255 -> PrimPath u -> [Primitive u]
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
                 => RGB255 -> PrimEllipse u -> [Primitive u]
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

