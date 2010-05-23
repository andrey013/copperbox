{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PictureInternal
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Internal representation of Pictures 
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.PictureInternal 
  (
  -- * Data types
    Picture(..)
  , DPicture
  , Primitive(..)
  , DPrimitive
  , Path(..)
  , DPath
  , PathSegment(..)
  , DPathSegment
  , Label(..)
  , DLabel
  , PrimEllipse(..)
  , DPrimEllipse

  , PathProps                   -- hide in Wumpus.Core export?
  , LabelProps                  -- hide in Wumpus.Core export?
  , EllipseProps                -- 
  , DrawPath(..)                -- hide in Wumpus.Core export?
  , DrawEllipse(..)
  , Locale  
 
  -- * Type class

  , PSUnit(..)
  
  -- * Extras
  , mapLocale
  , movePic
  , moveLocale
  , extractFrame
  , repositionProperties
  , ellipseControlPoints

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox
import Wumpus.Core.FontSize
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.OneList
import Wumpus.Core.TextEncodingInternal
import Wumpus.Core.Utils


import Data.AffineSpace
import Data.Semigroup

import Text.PrettyPrint.Leijen



-- | Picture is a leaf attributed tree - where attributes are 
-- colour, line-width etc. It is parametric on the unit type 
-- of points (typically Double).
-- 
-- Wumpus\'s leaf attributed tree, is not directly matched to 
-- PostScript\'s picture representation, which might be 
-- considered a node attributed tree (if you consider graphics
-- state changes less imperatively - setting attributes rather 
-- than global state change).
--
-- Considered as a node-attributed tree PostScript precolates 
-- graphics state updates downwards in the tree (vis-a-vis 
-- inherited attributes in an attibute grammar), where a 
-- graphics state change deeper in the tree overrides a higher 
-- one.
-- 
-- Wumpus on the other hand, simply labels each leaf with its
-- drawing attributes - there is no attribute inheritance.
-- When it draws the PostScript picture it does some 
-- optimization to avoid generating excessive graphics state 
-- changes in the PostScript code.
--
-- Apropos the constructors, Picture is a simple non-empty 
-- leaf-labelled rose tree via:
-- 
-- > Single (aka leaf) | Picture (OneList tree)
--
-- Where OneList is a variant of the standard list type that 
-- disallows empty lists.
-- 
-- The additional constructors are convenience:
--
-- @PicBlank@ has a bounding box but no content and is useful for
-- some picture language operations (e.g. @hsep@).
--
-- @Clip@ nests a picture (tree) inside a clipping path.
--


data Picture u = PicBlank (Locale u)
               | Single   (Locale u) (Primitive u)
               | Picture  (Locale u) (OneList (Picture u))
               | Clip     (Locale u) (Path u)      (Picture u)
  deriving (Eq,Show) 

type DPicture = Picture Double


-- | Wumpus\'s drawings are built from two fundamental 
-- primitives: paths (line segments and Bezier curves) and 
-- labels (single lines of text). 
-- 
-- Ellipses are a included as a primitive only for optimization 
-- - drawing a reasonable circle with Bezier curves needs at 
-- least eight curves. This is inconvenient for drawing dots 
-- which can otherwise be drawn with a single @arc@ command.
-- 
-- Wumpus does not follow PostScript and employ arcs as general 
-- path primitives - they are used only to draw ellipses. This 
-- is because arcs do not enjoy the nice properties of Bezier 
-- curves, whereby the affine transformation of a Bezier curve 
-- can simply be achieved by the affine transformation of it\'s 
-- control points.
--
-- Ellipses are represented by their center, half-width and 
-- half-height. Half-width and half-height are used so the 
-- bounding box can be calculated using only multiplication, and 
-- thus initially only obliging a Num constraint on the unit.
-- Though typically for affine transformations a Fractional 
-- constraint is also obliged.
--

data Primitive u = PPath    PathProps    (Path u)
                 | PLabel   LabelProps   (Label u) 
                 | PEllipse EllipseProps (PrimEllipse u)
  deriving (Eq,Show)

type DPrimitive = Primitive Double


-- | Path - start point and a list of path segments.
--
data Path u = Path (Point2 u) [PathSegment u]
  deriving (Eq,Show)

type DPath = Path Double

-- | PathSegment - either a cubic Bezier curve or a line.
--  
data PathSegment u = PCurveTo  (Point2 u) (Point2 u) (Point2 u)
                   | PLineTo   (Point2 u)
  deriving (Eq,Show)

type DPathSegment = PathSegment Double

-- | Label - represented by bottom left corner and text.
--
data Label u = Label 
      { label_bottom_left :: Point2 u
      , label_text        :: EncodedText
      , label_CTM         :: Matrix3'3 u
      }
  deriving (Eq,Show)

type DLabel = Label Double

-- Ellipse represented by center and half_width * half_height
--
data PrimEllipse u = PrimEllipse 
      { ellipse_center        :: Point2 u
      , ellipse_half_width    :: u
      , ellipse_half_height   :: u 
      , ellispe_CTM           :: Matrix3'3 u
      } 
  deriving (Eq,Show)

type DPrimEllipse = PrimEllipse Double

-- | Note when drawn /filled/ and drawn /stroked/ the same 
-- polygon will have (slightly) different size: 
-- 
-- * A filled shape fills /within/ the boundary of the shape
-- 
-- * A stroked shape draws a pen line around the boundary 
--   of the shape. The actual size depends on the thickness
--   of the line (stroke width).
--
data DrawPath = CFill | CStroke [StrokeAttr] | OStroke [StrokeAttr]
  deriving (Eq,Show)

-- | Ellipses and circles are always closed.
data DrawEllipse = EFill | EStroke [StrokeAttr]
  deriving (Eq,Show)

type PathProps    = (PSRgb, DrawPath)
type LabelProps   = (PSRgb, FontAttr)
type EllipseProps = (PSRgb, DrawEllipse)

-- | Locale = (current frame x bounding box)
-- 
-- Pictures (and sub-pictures) are located within an affine frame.
-- So pictures can be arranged (vertical and horizontal 
-- composition) their bounding box is cached.
--
-- In Wumpus, affine transformations (scalings, rotations...)
-- transform the frame rather than the constituent points of 
-- the primitives. Changes of frame are transmitted to PostScript
-- as @concat@ commands (and matrix transforms in SVG) - the 
-- @point-in-world-coordinate@ of a point on a path is never 
-- calculated.
--  
-- So that picture composition is remains stable under affine
-- transformation, the corners of bounding boxes are transformed
-- pointwise when the picture is scaled, rotated etc.
--
type Locale u = (Frame2 u, BoundingBox u) 


--------------------------------------------------------------------------------
-- Pretty printing

instance (Num u, Pretty u) => Pretty (Picture u) where
  pretty (PicBlank m)       = text "*BLANK*" <+> ppLocale m
  pretty (Single m prim)    = ppLocale m <$> indent 2 (pretty prim)
  pretty (Picture m ones)   = 
      ppLocale m <$> indent 2 (list $ toListF pretty ones)

  pretty (Clip m cpath p)   = 
      text "Clip:" <+> ppLocale m <$> indent 2 (pretty cpath)
                                  <$> indent 2 (pretty p)

ppLocale :: (Num u, Pretty u) => Locale u -> Doc
ppLocale (fr,bb) = align (ppfr <$> pretty bb) where
   ppfr = if standardFrame fr then text "*std-frame*" else pretty fr


instance Pretty u => Pretty (Primitive u) where
  pretty (PPath _ p)        = pretty "path:" <+> pretty p
  pretty (PLabel _ lbl)     = pretty lbl
  pretty (PEllipse _ e)     = pretty e 


instance Pretty u => Pretty (Path u) where
   pretty (Path pt ps) = pretty pt <> hcat (map pretty ps)

instance Pretty u => Pretty (PathSegment u) where
  pretty (PCurveTo p1 p2 p3)    = text ".*" <> pretty p1 <> text ",," <> pretty p2 
                                          <> text "*." <> pretty p3
  pretty (PLineTo pt)           = text "--" <> pretty pt

instance Pretty u => Pretty (Label u) where
  pretty (Label pt s ctm) = dquotes (pretty s) <> char '@' <> pretty pt
                                               <+> ppMatrixCTM ctm

instance Pretty u => Pretty (PrimEllipse u) where
  pretty (PrimEllipse c w h ctm) = pretty "ellipse" <+> pretty c
                                                    <+> text "w:" <> pretty w
                                                    <+> text "h:" <> pretty h
                                                    <+> ppMatrixCTM ctm

ppMatrixCTM :: Pretty u => Matrix3'3 u -> Doc
ppMatrixCTM = pp . toCTM where
  pp (CTM a b  c d  x y) = list $ map pretty [a,b,c,d,x,y]
  

--------------------------------------------------------------------------------

-- | Paths are sensibly a Semigroup - there is no notion of 
-- /empty path/.

instance Semigroup (Path u) where
  Path st xs `append` Path st' xs' = Path st (xs ++ (PLineTo st' : xs'))


instance Pointwise (Path u) where
  type Pt (Path u) = Point2 u
  pointwise f (Path st xs) = Path (f st) (map (pointwise f) xs)

instance Pointwise (PathSegment u) where
  type Pt (PathSegment u) = Point2 u
  pointwise f (PLineTo p)         = PLineTo (f p)
  pointwise f (PCurveTo p1 p2 p3) = PCurveTo (f p1) (f p2) (f p3)
  


--------------------------------------------------------------------------------
-- Affine trans instances

type instance DUnit (Picture u)     = u
type instance DUnit (Primitive u)   = u
type instance DUnit (Path u)        = u
type instance DUnit (PrimEllipse u) = u

instance (Num u, Ord u) => Transform (Picture u) where
  transform ctm pic = transformPicture (transform ctm) (transform ctm) pic


instance (Floating u, Real u) => Rotate (Picture u) where
  rotate = rotatePicture 

instance (Floating u, Real u) => RotateAbout (Picture u) where
  rotateAbout = rotatePictureAbout

instance (Num u, Ord u) => Scale (Picture u) where
  scale = scalePicture

instance (Num u, Ord u) => Translate (Picture u) where
  translate = translatePicture


-- Primitives

instance Num u => Transform (Primitive u) where
  transform ctm (PPath   attr path) = 
      PPath attr $ transformPath (transform ctm) path

  transform ctm (PLabel   attr lbl) = PLabel attr $ transformLabel ctm lbl

  transform ctm (PEllipse attr ell) = PEllipse attr $ transformEllipse ctm ell


instance (Real u, Floating u) => Rotate (Primitive u) where
  rotate ang (PPath    attr path) = PPath    attr $ rotatePath ang path
  rotate ang (PLabel   attr lbl)  = PLabel   attr $ rotateLabel ang lbl
  rotate ang (PEllipse attr ell)  = PEllipse attr $ rotateEllipse ang ell

instance (Real u, Floating u) => RotateAbout (Primitive u) where
  rotateAbout ang pt (PPath    attr path) = 
      PPath    attr $ rotatePathAbout ang pt path

  rotateAbout ang pt (PLabel   attr lbl)  = 
      PLabel   attr $ rotateLabelAbout ang pt lbl

  rotateAbout ang pt (PEllipse attr ell)  = 
      PEllipse attr $ rotateEllipseAbout ang pt ell


instance Num u => Scale (Primitive u) where
  scale x y (PPath    attr path) = PPath    attr $ scalePath x y path
  scale x y (PLabel   attr lbl)  = PLabel   attr $ scaleLabel x y lbl
  scale x y (PEllipse attr ell)  = PEllipse attr $ scaleEllipse x y ell

instance Num u => Translate (Primitive u) where
  translate x y (PPath    attr path) = PPath    attr $ translatePath x y path
  translate x y (PLabel   attr lbl)  = PLabel   attr $ translateLabel x y lbl
  translate x y (PEllipse attr ell)  = PEllipse attr $ translateEllipse x y ell

--------------------------------------------------------------------------------

-- Helpers for the affine transformations

rotatePicture :: (Real u, Floating u) => Radian -> Picture u -> Picture u
rotatePicture ang = transformPicture (rotate ang) (rotate ang)


rotatePictureAbout :: (Real u, Floating u) 
                   => Radian -> Point2 u -> Picture u -> Picture u
rotatePictureAbout ang pt = 
    transformPicture (rotateAbout ang pt) (rotateAbout ang pt)
  
scalePicture :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
scalePicture x y = transformPicture (scale x y) (scale x y)

translatePicture :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
translatePicture x y = transformPicture (translate x y) (translate x y)

-- TODO - the nameing for these functions is confusing now that
-- I've added a Transform typeclass.
--
-- Look to unifying the naming scheme in someway.
--
transformPicture :: (Num u, Ord u) 
                 => (Point2 u -> Point2 u) 
                 -> (Vec2 u -> Vec2 u) 
                 -> Picture u 
                 -> Picture u
transformPicture fp fv = 
    mapLocale $ \(frm,bb) -> (transformFrame fp fv frm, transformBBox fp bb)



transformFrame :: Num u
               => (Point2 u -> Point2 u) 
               -> (Vec2 u -> Vec2 u) 
               -> Frame2 u 
               -> Frame2 u
transformFrame fp fv (Frame2 e0 e1 o) = Frame2 (fv e0) (fv e1) (fp o)


-- Bounding boxes need recalculating after a transformation.
-- For instance after a reflection in the y-axis br becomes bl.
transformBBox :: (Num u, Ord u)
              => (Point2 u -> Point2 u) -> BoundingBox u -> BoundingBox u
transformBBox fp bb = trace $ map fp $ [bl,br,tl,tr]
  where
    (bl,br,tr,tl) = corners bb



-- Paths

rotatePath :: (Real u, Floating u) => Radian -> Path u -> Path u
rotatePath ang = transformPath (rotate ang)

rotatePathAbout :: (Real u, Floating u) 
                => Radian -> Point2 u -> Path u -> Path u
rotatePathAbout ang pt = transformPath (rotateAbout ang pt) 

scalePath :: Num u => u -> u -> Path u -> Path u
scalePath x y = transformPath (scale x y)

translatePath :: Num u => u -> u -> Path u -> Path u
translatePath x y = transformPath (translate x y)

transformPath :: (Point2 u -> Point2 u) -> Path u -> Path u
transformPath fp (Path start ss) = 
    Path (fp start) (map (transformPathSegment fp) ss)

-- Path Segments


transformPathSegment :: (Point2 u -> Point2 u) -> PathSegment u -> PathSegment u
transformPathSegment fp = pointwise fp

-- Labels

transformLabel :: Num u => Matrix3'3 u -> Label u -> Label u
transformLabel m33 (Label pt txt ctm) = Label pt txt (ctm * m33)

-- rotate CTM and pt or just CTM ??
rotateLabel :: (Real u, Floating u) => Radian -> Label u -> Label u
rotateLabel ang (Label pt txt ctm) = Label pt txt (ctm * rotationMatrix ang)

-- rotate CTM and pt or just CTM ??
rotateLabelAbout :: (Real u, Floating u) 
                => Radian -> Point2 u -> Label u -> Label u
rotateLabelAbout ang rpt (Label pt txt ctm) = 
    Label pt txt (ctm * originatedRotationMatrix ang rpt) 

scaleLabel :: Num u => u -> u -> Label u -> Label u
scaleLabel x y (Label pt txt ctm) = Label pt txt (ctm * scalingMatrix x y)

-- no need to change CTM for translation (??)
translateLabel :: Num u => u -> u -> Label u -> Label u
translateLabel x y (Label pt txt ctm) = Label (translate x y pt) txt ctm


-- 

transformEllipse :: Num u => Matrix3'3 u -> PrimEllipse u -> PrimEllipse u
transformEllipse m33 (PrimEllipse pt hw hh ctm) = 
    PrimEllipse pt hw hh (ctm * m33)

rotateEllipse :: (Real u, Floating u) 
              => Radian -> PrimEllipse u -> PrimEllipse u
rotateEllipse ang (PrimEllipse pt hw hh ctm) = 
    PrimEllipse pt hw hh (ctm * rotationMatrix ang)

rotateEllipseAbout :: (Real u, Floating u) 
                   => Radian -> Point2 u -> PrimEllipse u -> PrimEllipse u
rotateEllipseAbout ang rpt (PrimEllipse pt hw hh ctm) = 
    PrimEllipse pt hw hh (ctm * originatedRotationMatrix ang rpt)


scaleEllipse :: Num u => u -> u -> PrimEllipse u -> PrimEllipse u
scaleEllipse x y (PrimEllipse pt hw hh ctm) = 
    PrimEllipse pt hw hh (ctm * scalingMatrix x y)


translateEllipse :: Num u => u -> u -> PrimEllipse u -> PrimEllipse u
translateEllipse x y (PrimEllipse pt hw hh ctm) = 
    PrimEllipse (translate x y pt) hw hh ctm

--------------------------------------------------------------------------------
-- Boundary

instance Boundary (Picture u) where
  boundary (PicBlank (_,bb))     = bb
  boundary (Single   (_,bb) _)   = bb
  boundary (Picture  (_,bb) _)   = bb
  boundary (Clip     (_,bb) _ _) = bb

instance (Num u, Ord u) => Boundary (Path u) where
  boundary (Path st xs) = trace $ st : foldr f [] xs where
      f (PLineTo p1)        acc  = p1 : acc
      f (PCurveTo p1 p2 p3) acc  = p1 : p2 : p3 : acc 


-- Note - this will calculate an approximate bounding box for 
-- text.

instance (Fractional u, Floating u, Ord u) => Boundary (Primitive u) where
  boundary (PPath _ p)        = boundary p
  boundary (PLabel (_,a) l)   = primLabelBoundary a l 
  boundary (PEllipse _ e)     = boundary e

primLabelBoundary :: (Fractional u, Ord u) 
                  => FontAttr -> Label u -> BoundingBox u
primLabelBoundary attr (Label pt xs ctm) = retrace (ctm *#) untraf_bbox
  where
    untraf_bbox = textBounds (font_size attr) pt char_count
    char_count  = textLength xs

instance (Floating u, Ord u) => Boundary (PrimEllipse u) where
  boundary = ellipseBoundary

-- Find the bbox of an ellipse by drawing it as four bezier 
-- curves then trace all the points and control points to find
-- the bbox.
-- 
-- Note all_points takes three of the four points to avoid 
-- duplicating
-- /matched/ start-end points
--

ellipseBoundary :: (Floating u, Ord u) => PrimEllipse u -> BoundingBox u
ellipseBoundary = trace . ellipseControlPoints

-- PROBLEM:
-- Currently a rotated circle has a different BBox to a 
-- non-rotated circle, because of how tangents are selected...
-- 
-- This is the same as a diamond having a larger BBox
-- than a square with same side-length
--

--------------------------------------------------------------------------------
--


mapLocale :: (Locale u -> Locale u) -> Picture u -> Picture u
mapLocale f (PicBlank m)      = PicBlank (f m)
mapLocale f (Single   m prim) = Single (f m) prim
mapLocale f (Picture  m ones) = Picture (f m) ones
mapLocale f (Clip     m x p)  = Clip (f m) x p


movePic :: Num u => Vec2 u -> Picture u -> Picture u
movePic v = mapLocale (moveLocale v) 

  
moveLocale :: Num u => Vec2 u -> Locale u -> Locale u
moveLocale v (fr,bb) = (displaceOrigin v fr, pointwise (.+^ v) bb) 

--------------------------------------------------------------------------------


-- | Should this really be public?
extractFrame :: Num u => Picture u -> Frame2 u
extractFrame (PicBlank (fr,_))     = fr
extractFrame (Single   (fr,_) _)   = fr
extractFrame (Picture  (fr,_) _)   = fr
extractFrame (Clip     (fr,_) _ _) = fr


-- This needs is for PostScript and SVG output - it should be 
-- hidden in the export list of Wumpus.Core


-- If a picture has coordinates smaller than (P2 4 4) then it 
-- needs repositioning before it is drawn to PostSCript or SVG.
-- 
-- (P2 4 4) gives a 4 pt margin - maybe it sould be (0,0) or 
-- user defined.
--
repositionProperties :: (Num u, Ord u) => Picture u -> (BoundingBox u, Maybe (Vec2 u))
repositionProperties = fn . boundary where
  fn bb@(BBox (P2 llx lly) (P2 urx ury))
      | llx < 4 || lly < 4  = (BBox ll ur, Just $ V2 x y)
      | otherwise           = (bb, Nothing)
    where 
      x  = 4 - llx
      y  = 4 - lly
      ll = P2 (llx+x) (lly+y)
      ur = P2 (urx+x) (ury+y)  


-- | Get the control points as a list
-- 
-- There are no duplicates in the list except for the final 
-- /wrap-around/. We take 4 points initially (start,cp1,cp2,end)
-- then (cp1,cp2,end) for the other three quadrants.
--
ellipseControlPoints :: (Floating u, Ord u)
                     => PrimEllipse u -> [Point2 u]
ellipseControlPoints (PrimEllipse ctr hw hh ctm) = map (new_mtrx *#) circ
  where
    (radius,(dx,dy)) = circleScalingProps hw hh
    new_mtrx         = ctm * scalingMatrix dx dy
    circ             = bezierCircle 1 radius ctr

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



