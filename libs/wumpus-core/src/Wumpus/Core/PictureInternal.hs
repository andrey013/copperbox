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

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox
import Wumpus.Core.FontSize
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.TextEncodingInternal
import Wumpus.Core.Utils


import Data.AffineSpace
import Data.Semigroup

import Control.Applicative ( liftA2 )

import Text.PrettyPrint.Leijen



-- | Picture is a leaf attributed tree - where atttibutes are 
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
      }
  deriving (Eq,Show)

type DLabel = Label Double

-- Ellipse represented by center and half_width * half_height
--
data PrimEllipse u = PrimEllipse 
      { ellipse_center      :: Point2 u
      , ellipse_half_width  :: u
      , ellipse_half_height :: u 
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
  pretty (Picture m ones)  = 
      ppLocale m <$> indent 2 (list $ toListWith pretty ones)

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
  pretty (Label pt s) = dquotes (pretty s) <> char '@' <> pretty pt

instance Pretty u => Pretty (PrimEllipse u) where
  pretty (PrimEllipse c w h) = pretty "ellipse" <+> pretty c
                                                <+> text "w:" <> pretty w
                                                <+> text "h:" <> pretty h


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


instance (Floating u, Real u) => Rotate (Picture u) where
  rotate = rotatePicture 

instance (Floating u, Real u) => RotateAbout (Picture u) where
  rotateAbout = rotatePictureAbout

instance (Num u, Ord u) => Scale (Picture u) where
  scale = scalePicture

instance (Num u, Ord u) => Translate (Picture u) where
  translate = translatePicture

--------------------------------------------------------------------------------

-- Helpers for the affine transformations

rotatePicture :: (Real u, Floating u) => Radian -> Picture u -> Picture u
rotatePicture = liftA2 transformPicture rotate rotate


rotatePictureAbout :: (Real u, Floating u) 
                   => Radian -> Point2 u -> Picture u -> Picture u
rotatePictureAbout ang pt = 
    transformPicture (rotateAbout ang pt) (rotateAbout ang pt)
  
scalePicture :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
scalePicture x y = transformPicture (scale x y) (scale x y)

translatePicture :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
translatePicture x y = transformPicture (translate x y) (translate x y)


transformPicture :: (Num u, Ord u) 
                 => (Point2 u -> Point2 u) 
                 -> (Vec2 u -> Vec2 u) 
                 -> Picture u 
                 -> Picture u
transformPicture fp fv = 
    mapLocale $ \(frm,bb) -> (transformFrame fp fv frm, transformBBox fp bb)


-- Shouldn't transforming the frame be the inverse transformation?

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



--------------------------------------------------------------------------------
-- Boundary

instance (Num u, Ord u) => Boundary (Path u) where
  boundary (Path st xs) = trace $ st : foldr f [] xs where
      f (PLineTo p1)        acc  = p1 : acc
      f (PCurveTo p1 p2 p3) acc  = p1 : p2 : p3 : acc 


-- Note - this will calculate a very bad bounding box for text.
-- Descenders will be transgress the boundary and width will be 
-- very long.

instance (Fractional u, Ord u) => Boundary (Primitive u) where
  boundary (PPath _ p)                  = boundary p
  boundary (PLabel (_,a) (Label pt xs)) = textBounds (font_size a) pt char_count
    where char_count = textLength xs
  boundary (PEllipse _ e)               = boundary e


instance (Fractional u, Ord u) => Boundary (PrimEllipse u) where
  boundary (PrimEllipse c hw hh)        = BBox (c .-^ v) (c .+^ v) 
    where v = V2 hw hh
 



instance Boundary (Picture u) where
  boundary (PicBlank (_,bb))     = bb
  boundary (Single   (_,bb) _)   = bb
  boundary (Picture  (_,bb) _)   = bb
  boundary (Clip     (_,bb) _ _) = bb





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


