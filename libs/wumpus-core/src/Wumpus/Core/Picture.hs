{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Picture
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Construction of pictures, paths and text labels.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.Picture 
  (
  
   -- * Construction
    blankPicture
  , frame
  , frameWithin
  , frameMulti
  , multi


  , path
  , lineTo
  , curveTo
  , vertexPath  
  , curvedPath

  , wumpus_default_font

  -- * Constructing primitives
  , Stroke(..)
  , zostroke
  , zcstroke

  , Fill(..)
  , zfill
  
  , clip

  , TextLabel(..)
  , ztextlabel

  , Ellipse(..)
  , zellipse

  -- * Operations
  , extendBoundary


  -- * Minimal - picture composition
  , picMoveBy
  , picOver
  , picBeside
  , illustrateBounds
  , illustrateBoundsPrim 
  , illustrateControlPoints

  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.OneList
import Wumpus.Core.PictureInternal
import Wumpus.Core.TextEncodingInternal

import Data.Semigroup


--------------------------------------------------------------------------------

-- Default attributes

psBlack :: PSRgb
psBlack = RGB3 0 0 0
 
-- aka the standard frame
stdFrame :: Num u => Frame2 u 
stdFrame = ortho zeroPt


--------------------------------------------------------------------------------
-- Construction



-- | Create a blank 'Picture' sized to the supplied bounding box.
-- This is useful for spacing rows or columns of pictures.
--
blankPicture :: Num u => BoundingBox u -> Picture u
blankPicture bb = PicBlank (stdFrame, bb)


-- | Lift a 'Primitive' to a 'Picture', located in the standard frame.
--
frame :: (Fractional u, Floating u, Ord u) => Primitive u -> Picture u
frame p = Single (stdFrame, boundary p) p 

-- | Frame a picture within the supplied bounding box
-- 
-- A text label uses the supplied bounding box as is - no 
-- clipping is performed if the bounding box is 
-- smaller than the boundary size of the text. This may 
-- cause strange overlap for subsequent composite pictures, and
-- incorrect bounding box annotations in the prologue of the 
-- generated EPS file. 
-- 
-- Paths and ellipses are bound within the union of the supplied 
-- bounding box and the inherent bounding box or the path or 
-- ellipse. Thus the bounding box will never reframed to a 
-- smaller size than the /natural/ bounding box.
--
frameWithin :: (Fractional u, Floating u, Ord u) 
            => Primitive u -> BoundingBox u -> Picture u
frameWithin p@(PLabel _ _) bb = Single (stdFrame,bb) p
frameWithin p              bb = Single (stdFrame,bb `append` boundary p) p




-- | Lift a list of primitives to a composite picture, all 
-- primitives will be located within the standard frame.
--
-- The order of the list maps to the zorder - the front of the
-- list is drawn at the top.
--
-- This function throws an error when supplied the empty list.
--
frameMulti :: (Fractional u, Floating u, Ord u) 
           => [Primitive u] -> Picture u
frameMulti [] = error "Wumpus.Core.Picture.frameMulti - empty list"
frameMulti xs = multi $ map frame xs


-- | Place multiple pictures within the same affine frame.
--
-- This function throws an error when supplied the empty list.
--
multi :: (Fractional u, Ord u) => [Picture u] -> Picture u
multi ps = Picture (stdFrame, sconcat $ map boundary ps) $ step ps
  where 
    sconcat []      = error err_msg
    sconcat (x:xs)  = foldr append x xs

    step [x]        = one x
    step (x:xs)     = x `cons` step xs
    step _          = error err_msg

    err_msg         = "Wumpus.Core.Picture.multi - empty list"



-- | Create a Path from a start point and a list of 
-- PathSegments.
path :: Point2 u -> [PathSegment u] -> Path u
path = Path 

-- | Create a straight-line PathSegment.
--
lineTo :: Point2 u -> PathSegment u
lineTo = PLineTo

-- | Create a curved PathSegment.
--
curveTo :: Point2 u -> Point2 u -> Point2 u -> PathSegment u
curveTo = PCurveTo


-- | Convert the list of vertices to a path of straight line 
-- segments.
--
vertexPath :: [Point2 u] -> Path u
vertexPath []     = error "Picture.vertexPath - empty point list"
vertexPath (x:xs) = Path x (map PLineTo xs)



-- | Convert a list of vertices to a path of curve segments.
-- The first point in the list makes the start point, each curve 
-- segment thereafter takes 3 points. /Spare/ points at the end 
-- are discarded. 
--
curvedPath :: [Point2 u] -> Path u
curvedPath []     = error "Picture.curvedPath - empty point list"
curvedPath (x:xs) = Path x (fn xs) where
  fn (a:b:c:ys) = PCurveTo a b c : fn ys 
  fn _          = []


  


-- | Constant for the default font, which is @Courier@ (aliased 
-- to @Courier New@ for SVG).
-- 
-- The font size is 24 point. Note that only a handful of font 
-- sizes are available directly to PostScript / GhostScript.
--
-- To get non-standard sizes, consider drawing the text and 
-- applying a 'uniformScale'.
--
wumpus_default_font :: FontAttr
wumpus_default_font = FontAttr "Courier" "Courier New" SVG_REGULAR 24


--------------------------------------------------------------------------------
-- Take Paths to Primitives


ostrokePath :: (Num u, Ord u) 
            => PSRgb -> [StrokeAttr] -> Path u -> Primitive u
ostrokePath c attrs p = PPath (c, OStroke attrs) p

cstrokePath :: (Num u, Ord u) 
            => PSRgb -> [StrokeAttr] -> Path u -> Primitive u
cstrokePath c attrs p = PPath (c, CStroke attrs) p

-- | Create a open, stroked path (@ostroke@) or a closed, stroked
-- path (@cstroke@).
--
-- @ostroke@ and @cstroke@ are overloaded to make attributing 
-- the path more convenient.
-- 
class Stroke t where
  ostroke :: (Num u, Ord u) => t -> Path u -> Primitive u
  cstroke :: (Num u, Ord u) => t -> Path u -> Primitive u

instance Stroke () where
  ostroke () = ostrokePath psBlack []
  cstroke () = cstrokePath psBlack []

instance Stroke (RGB3 Double) where
  ostroke c = ostrokePath (psColour c) []
  cstroke c = cstrokePath (psColour c) []

instance Stroke (HSB3 Double) where
  ostroke c = ostrokePath (psColour c) []
  cstroke c = cstrokePath (psColour c) []

instance Stroke (Gray Double) where
  ostroke c = ostrokePath (psColour c) []
  cstroke c = cstrokePath (psColour c) []


instance Stroke StrokeAttr where
  ostroke x = ostrokePath psBlack [x]
  cstroke x = cstrokePath psBlack [x]

instance Stroke [StrokeAttr] where
  ostroke xs = ostrokePath psBlack xs
  cstroke xs = cstrokePath psBlack xs



instance Stroke (RGB3 Double,StrokeAttr) where
  ostroke (c,x) = ostrokePath (psColour c) [x]
  cstroke (c,x) = cstrokePath (psColour c) [x]

instance Stroke (HSB3 Double,StrokeAttr) where
  ostroke (c,x) = ostrokePath (psColour c) [x]
  cstroke (c,x) = cstrokePath (psColour c) [x]

instance Stroke (Gray Double,StrokeAttr) where
  ostroke (c,x) = ostrokePath (psColour c) [x]
  cstroke (c,x) = cstrokePath (psColour c) [x]

instance Stroke (RGB3 Double,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath (psColour c) xs
  cstroke (c,xs) = cstrokePath (psColour c) xs

instance Stroke (HSB3 Double,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath (psColour c) xs
  cstroke (c,xs) = cstrokePath (psColour c) xs

instance Stroke (Gray Double,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath (psColour c) xs
  cstroke (c,xs) = cstrokePath (psColour c) xs


-- | Create an open stoke coloured black.
--
zostroke :: (Num u, Ord u) => Path u -> Primitive u
zostroke = ostrokePath psBlack []
 
-- | Create a closed stroke coloured black.
--
zcstroke :: (Num u, Ord u) => Path u -> Primitive u
zcstroke = cstrokePath psBlack []







fillPath :: (Num u, Ord u) => PSRgb -> Path u -> Primitive u
fillPath c p = PPath (c,CFill) p

-- | Create a filled path (@fill@). Fills only have one 
-- property - colour. But there are various representations of 
-- colour.
--
-- @ fill () @ will fill with the default colour - black.
-- 
class Fill t where
  fill :: (Num u, Ord u) => t -> Path u -> Primitive u
 

instance Fill ()                where fill () = fillPath psBlack 
instance Fill (RGB3 Double)     where fill = fillPath . psColour
instance Fill (HSB3 Double)     where fill = fillPath . psColour
instance Fill (Gray Double)     where fill = fillPath . psColour

-- | Create a filled path coloured black. 
zfill :: (Num u, Ord u) => Path u -> Primitive u
zfill = fillPath psBlack

--------------------------------------------------------------------------------
-- Clipping 

-- | Clip a picture with respect to the supplied path.
--
clip :: (Num u, Ord u) => Path u -> Picture u -> Picture u
clip cp p = Clip (ortho zeroPt, boundary cp) cp p


--------------------------------------------------------------------------------
-- Labels to primitive

mkTextLabel :: Num u => PSRgb -> FontAttr -> String -> Point2 u -> Primitive u
mkTextLabel c attr txt pt = PLabel (c,attr) lbl 
  where
    lbl = Label pt (lexLabel txt) identityMatrix


-- | Create a text label. The string should not contain newline
-- or tab characters. Use 'multilabel' to create text with 
-- multiple lines.
-- 
-- @textlabel@ is overloaded to make attributing the label more 
-- convenient.
--
-- Unless a 'FontAttr' is specified, the label will use 12pt 
-- Courier.
--
-- The supplied point is is the bottom left corner.
--
class TextLabel t where 
  textlabel :: Num u => t -> String -> Point2 u -> Primitive u


instance TextLabel () where 
    textlabel () = mkTextLabel psBlack wumpus_default_font

instance TextLabel (RGB3 Double) where
  textlabel c = mkTextLabel (psColour c) wumpus_default_font

instance TextLabel (HSB3 Double) where
  textlabel c = mkTextLabel (psColour c) wumpus_default_font

instance TextLabel (Gray Double) where
  textlabel c = mkTextLabel (psColour c) wumpus_default_font

instance TextLabel FontAttr where
  textlabel a = mkTextLabel psBlack a

instance TextLabel (RGB3 Double,FontAttr) where
  textlabel (c,a) = mkTextLabel (psColour c) a

instance TextLabel (HSB3 Double,FontAttr) where
  textlabel (c,a) = mkTextLabel (psColour c) a

instance TextLabel (Gray Double,FontAttr) where
  textlabel (c,a) = mkTextLabel (psColour c) a

-- | Create a label where the font is @Courier@, text size is 24pt
-- and colour is black.
ztextlabel :: Num u => String -> Point2 u -> Primitive u
ztextlabel = mkTextLabel psBlack wumpus_default_font



--------------------------------------------------------------------------------

mkEllipse :: Num u 
          => PSRgb -> DrawEllipse -> u -> u -> Point2 u -> Primitive u
mkEllipse c dp hw hh pt = PEllipse (c,dp) (PrimEllipse pt hw hh identityMatrix)


ellipseDefault :: EllipseProps
ellipseDefault = (psBlack, EFill)


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
instance Ellipse DrawEllipse    where ellipse dp = mkEllipse psBlack dp

instance Ellipse StrokeAttr     where 
    ellipse = mkEllipse psBlack . EStroke . return

instance Ellipse [StrokeAttr]   where 
    ellipse = mkEllipse psBlack . EStroke

instance Ellipse (RGB3 Double) where 
    ellipse c = mkEllipse (psColour c) EFill

instance Ellipse (HSB3 Double) where 
    ellipse c = mkEllipse (psColour c) EFill

instance Ellipse (Gray Double) where 
    ellipse c = mkEllipse (psColour c) EFill


instance Ellipse (RGB3 Double,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse (psColour c) dp

instance Ellipse (HSB3 Double,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse (psColour c) dp

instance Ellipse (Gray Double,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse (psColour c) dp


instance Ellipse (RGB3 Double,StrokeAttr) where 
    ellipse (c,x) = mkEllipse (psColour c) (EStroke [x])

instance Ellipse (HSB3 Double,StrokeAttr) where 
    ellipse (c,x) = mkEllipse (psColour c) (EStroke [x])

instance Ellipse (Gray Double,StrokeAttr) where 
    ellipse (c,x) = mkEllipse (psColour c) (EStroke [x])

instance Ellipse (RGB3 Double,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse (psColour c) (EStroke xs)

instance Ellipse (HSB3 Double,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse (psColour c) (EStroke xs)

instance Ellipse (Gray Double,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse (psColour c) (EStroke xs)


-- | Create a black, filled ellipse. 
zellipse :: Num u => u -> u -> Point2 u -> Primitive u
zellipse = uncurry mkEllipse ellipseDefault


--------------------------------------------------------------------------------

-- Operations on pictures and paths




-- | Extend the bounding box of a picture. 
--
-- The bounding box is both horizontal directions by @x@ and 
-- both vertical directions by @y@. @x@ and @y@ must be positive
-- This function cannot be used to shrink a boundary.
--
extendBoundary :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
extendBoundary x y = mapLocale (\(fr,bb) -> (fr, extBB (posve x) (posve y) bb)) 
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
-- Draw the first picture on to off the second picture - 
-- neither picture will be moved.
--
picOver :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `picOver` b = Picture (ortho zeroPt, bb) (cons b $ one a) 
  where
    bb = union (boundary a) (boundary b)

-- | 'picMoveBy' : @ picture -> vector -> picture @
-- 
--  Move a picture by the supplied vector. 
--
picMoveBy :: Num u => Picture u -> Vec2 u -> Picture u
p `picMoveBy` v = v `movePic` p 

-- | 'picBeside' : @ picture -> picture -> picture @
--
-- Move the second picture to sit at the right side of the
-- first picture
--
picBeside :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `picBeside` b = a `picOver` (b `picMoveBy` v) 
  where 
    v = hvec $ rightPlane (boundary a) - leftPlane (boundary b) 

-- | 'illustrateBounds' : @ colour -> picture -> picture @
-- 
-- Draw the picture on top of an image of its bounding box.
-- The bounding box image will be drawn in the supplied colour.
--
illustrateBounds :: (Floating u, Ord u) => DRGB -> Picture u -> Picture u
illustrateBounds rgb p = p `picOver` (frameMulti $ boundsPrims rgb p) 


-- | 'illustrateBoundsPrim' : @ colour -> primitive -> picture @
-- 
-- Draw the primitive on top of an image of its bounding box.
-- The bounding box image will be drawn in the supplied colour.
--
-- The result will be lifted from Primitive to Picture.
-- 
illustrateBoundsPrim :: (Floating u, Ord u) 
                     => DRGB -> Primitive u -> Picture u
illustrateBoundsPrim rgb p = frameMulti (boundsPrims rgb p ++ [p])

-- Note - above has to use snoc (++ [p]) to get the picture to
-- draw above the bounding box image.


-- | Draw a the rectangle of a bounding box, plus cross lines
-- joining the corners.
--
boundsPrims :: (Num u, Ord u, Boundary t, u ~ DUnit t) 
            => DRGB -> t -> [Primitive u]
boundsPrims rgb a = [ bbox_rect, bl_to_tr, br_to_tl ]
  where
    (bl,br,tr,tl) = corners $ boundary a
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
illustrateControlPoints :: (Floating u, Ord u)
                        => DRGB -> Primitive u -> Picture u
illustrateControlPoints rgb prim = step prim
  where
    step (PEllipse _ e) = frameMulti (prim : ellipseCtrlLines rgb e)
    step (PPath    _ p) = frameMulti (prim : pathCtrlLines rgb p)
    step _              = frame prim

-- Genrate lines illustrating the control points of curves on 
-- a Path.
--
-- Two lines are generated for a Bezier curve:
-- start-point to control-point1; control-point2 to end-point
--
-- Nothing is generated for a straight line.
--
pathCtrlLines :: (Num u, Ord u) => DRGB -> Path u -> [Primitive u]
pathCtrlLines rgb (Path start ss) = step start ss
  where 
    -- trail the current end point through the recursion...
    step _ []                    = []
    step _ (PLineTo e:xs)        = step e xs
    step s (PCurveTo c1 c2 e:xs) = mkLine s c1 : mkLine c2 e : step e xs 

    mkLine s e                   = ostroke rgb (Path s [lineTo e]) 


-- Generate lines illustrating the control points of an 
-- ellipse:
-- 
-- Two lines for each quadrant: 
-- start-point to control-point1; control-point2 to end-point
--
ellipseCtrlLines :: (Floating u, Ord u) 
                     => DRGB -> PrimEllipse u -> [Primitive u]
ellipseCtrlLines rgb pe = start all_points
  where 
    -- list in order: 
    -- [s,cp1,cp2,e, cp1,cp2,e, cp1,cp2,e, cp1,cp2,e]

    all_points           = ellipseControlPoints pe

    start (s:c1:c2:e:xs) = mkLine s c1 : mkLine c2 e : rest e xs
    start _              = []

    rest s (c1:c2:e:xs)  = mkLine s c1 : mkLine c2 e : rest e xs
    rest _ _             = []

    mkLine s e           = ostroke rgb (Path s [lineTo e]) 


