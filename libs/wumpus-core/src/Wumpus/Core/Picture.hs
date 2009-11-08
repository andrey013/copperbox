{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Picture
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.Picture 
  (
  -- * Data types
    Picture(..)
  , DPicture
  , Primitive(..)
  , DPrimitive
  , Path(..)
  , DPath
  , PathSeg(..)
  , DPathSeg
  , Label(..)
  , DLabel

  , PathProps                   -- hide in Wumpus.Core export?
  , LabelProps                  -- hide in Wumpus.Core export?
  , EllipseProps                -- 
  , DrawProp(..)                -- hide in Wumpus.Core export?
  , DrawEllipse(..)
  
   -- * Construction
  , empty

  , frame
  , multi

  , Stroke(..)
  , zostroke
  , zcstroke

  , Fill(..)
  , zfill

  , TextLabel(..)
  , ztextlabel

  , Ellipse(..)
  , zellipse


  , vertexPath  




  -- * Operations
  , nullPicture
  , extractFrame

  
  , repositionProperties

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox hiding ( center )
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.PictureLanguage

import Data.FunctionExtras
import Data.Groupoid

import Data.AffineSpace

import Text.PrettyPrint.Leijen hiding ( empty )

import Data.Monoid



-- | Picture is a leaf attributed tree - where atttibutes are 
-- colour, line-width etc.
-- 
-- This is not an ideal match to PostScript, which might be 
-- considered a node attributed tree if you recast graphics
-- state updates as syntactic commands encountered during 
-- top-down evaluation.
-- 
-- Currently this mismatch means that the PostScript code 
-- generated by Wumpus had significant overuse of PostScript's
-- @gsave@ and @grestore@.
--
-- At some point a tree-rewriting step might be added to 
-- coalesce some of the repeated graphics state updates.
--

data Picture u = Empty
               | Single   (Measure u) (Primitive u)
               | Multi    (Measure u) [Primitive u] -- multiple prims in same affine frame
               | Picture  (Measure u) (Picture u)   (Picture u)
               | Clip     (Measure u) (Path u)      (Picture u)
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
-- curves - the affine transformation of a Bezier curve is the 
-- the same as the affine transformation of it\'s control points.
--
-- Ellipses are represented by their center, half-width and 
-- half-height. Half-width and half-height are used so the 
-- bounding box can be calculated using only multiplication, and 
-- thus generally only obliging a Num constraint on the unit.
-- Though typically for affine transformations a Fractional 
-- constraint is also obliged.
--

data Primitive u = Path1    PathProps (Path u)
                 | Label1   LabelProps (Label u) 
                 | Ellipse1 { 
                      ellipse_props       :: EllipseProps,
                      ellipse_center      :: Point2 u,
                      ellipse_half_width  :: u,
                      ellipse_half_height :: u 
                    } 
  deriving (Eq,Show)

type DPrimitive = Primitive Double



data Path u = Path (Point2 u) [PathSeg u]
  deriving (Eq,Show)

type DPath = Path Double


data PathSeg u = PCurve  (Point2 u) (Point2 u) (Point2 u)
               | PLine   (Point2 u)
  deriving (Eq,Show)

type DPathSeg = PathSeg Double

data Label u = Label { 
                   label_bottom_left :: Point2 u,
                   label_text        ::  String
                 }
  deriving (Eq,Show)

type DLabel = Label Double


-- | Note when drawn /filled/ and drawn /stroked/ the same 
-- polygon will have (slightly) different size: 
-- 
-- * A filled shape fills /within/ the boundary of the shape
-- 
-- * A stroked shape draws a pen line around the boundary 
--   of the shape. The actual size depends on the thickness
--   of the line (stroke width).
--
data DrawProp = CFill | CStroke [StrokeAttr] | OStroke [StrokeAttr]
  deriving (Eq,Show)

-- | Ellipses and circles are always closed.
data DrawEllipse = EFill | EStroke [StrokeAttr]
  deriving (Eq,Show)

type PathProps    = (PSColour, DrawProp)
type LabelProps   = (PSColour, FontAttr)
type EllipseProps = (PSColour, DrawEllipse)

-- | Measure = (_current_ frame x bounding box)
type Measure u = (Frame2 u, BoundingBox u) 





--------------------------------------------------------------------------------
-- Pretty printing

instance (Num u, Pretty u) => Pretty (Picture u) where
  pretty Empty              = text "*empty*"
  pretty (Single m prim)    = ppMeasure m <$> indent 2 (pretty prim)

  pretty (Multi m prims)    = ppMeasure m <$> indent 2 (list $ map pretty prims)
  pretty (Picture m pl pr)  = 
      ppMeasure m <$> indent 2 (text "LEFT" <+> pretty pl)
                  <$> indent 2 (text "RGHT" <+> pretty pr)

  pretty (Clip m cpath p)   = 
      text "Clip:" <+> ppMeasure m <$> indent 2 (pretty cpath)
                                   <$> indent 2 (pretty p)

ppMeasure :: (Num u, Pretty u) => Measure u -> Doc
ppMeasure (fr,bbox) = align (ppfr <$> pretty bbox) where
   ppfr = if standardFrame fr then text "*std-frame*" else pretty fr


instance Pretty u => Pretty (Primitive u) where
  pretty (Path1 _ p)        = pretty "path:" <+> pretty p
  pretty (Label1 _ lbl)     = pretty lbl
  pretty (Ellipse1 _ c w h) = pretty "ellipse" <+> pretty c
                                               <+> text "w:" <> pretty w
                                               <+> text "h:" <> pretty h


instance Pretty u => Pretty (Path u) where
   pretty (Path pt ps) = pretty pt <> hcat (map pretty ps)

instance Pretty u => Pretty (PathSeg u) where
  pretty (PCurve p1 p2 p3)    = text ".*" <> pretty p1 <> text ",," <> pretty p2 
                                          <> text "*." <> pretty p3
  pretty (PLine pt)           = text "--" <> pretty pt

instance Pretty u => Pretty (Label u) where
  pretty (Label pt s) = dquotes (text s) <> char '@' <> pretty pt


--------------------------------------------------------------------------------

-- | Paths are sensibly a Groupoid - there is no notion of 
-- /empty path/.

instance Groupoid (Path u) where
  Path st xs `gappend` Path st' xs' = Path st (xs ++ (PLine st' : xs'))


instance Pointwise (Path u) where
  type Pt (Path u) = Point2 u
  pointwise f (Path st xs) = Path (f st) (map (pointwise f) xs)

instance Pointwise (PathSeg u) where
  type Pt (PathSeg u) = Point2 u
  pointwise f (PLine p)         = PLine (f p)
  pointwise f (PCurve p1 p2 p3) = PCurve (f p1) (f p2) (f p3)
  

instance (Floating u, Real u) => Rotate (Picture u) where
  rotate = rotatePicture 

instance (Floating u, Real u) => RotateAbout (Picture u) where
  type RotateAboutUnit (Picture u) = u
  rotateAbout = rotatePictureAbout

instance (Num u, Ord u) => Scale (Picture u) where
  type ScaleUnit (Picture u) = u
  scale = scalePicture

instance (Num u, Ord u) => Translate (Picture u) where
  type TranslateUnit (Picture u) = u
  translate = translatePicture

--------------------------------------------------------------------------------

-- Helpers for the affine transformations

rotatePicture :: (Real u, Floating u) => Radian -> Picture u -> Picture u
rotatePicture = subst' transformPicture rotate rotate


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
    mapMeasure $ prod (transformFrame fp fv) (transformBBox fp)


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
transformBBox fp = trace . map fp . corners


--------------------------------------------------------------------------------

-- TO DETERMINE
-- What should leftBound and rightBound be for an empty picture?

instance (Num u, Ord u) => Horizontal (Picture u) where
  type HUnit (Picture u) = u

  moveH a    = movePic (hvec a) 
  leftBound  = maybe 0 id . leftPlane . boundary
  rightBound = maybe 0 id . rightPlane . boundary

instance (Num u, Ord u) => Vertical (Picture u) where
  type VUnit (Picture u) = u

  moveV a     = movePic (vvec a) 
  topBound    = maybe 0 id . upperPlane . boundary
  bottomBound = maybe 0 id . lowerPlane . boundary

instance (Num u, Ord u) => Composite (Picture u) where
  cempty  = empty

  a     `composite` Empty = a
  Empty `composite` b     = b
  a     `composite` b     = Picture (frameDefault, bb) a b where
                            bb = union (boundary a) (boundary b)



instance (Num u, Ord u, Horizontal (Picture u), Vertical (Picture u),
          HUnit (Picture u) ~ VUnit (Picture u)) => 
      PMove (Picture u) where
  pmove x y = movePic (V2 x y)

--------------------------------------------------------------------------------
-- Boundary

instance (Num u, Ord u) => Boundary (Path u) where
  type BoundaryUnit (Path u) = u
  boundary (Path st xs) = trace $ st : foldr f [] xs where
      f (PLine p1)        acc  = p1 : acc
      f (PCurve p1 p2 p3) acc  = p1 : p2 : p3 : acc 


-- Note - this will calculate a very bad bounding box for text.
-- Descenders will be clipped and width will be very long.

instance (Num u, Ord u) => Boundary (Primitive u) where
  type BoundaryUnit (Primitive u) = u
  boundary (Path1 _ p)                  = boundary p
  boundary (Label1 (_,a) (Label pt xs)) = BBox pt (pt .+^ (V2 w h))
    where w = fromIntegral $ length xs * font_size a
          h = fromIntegral $ font_size a
  boundary (Ellipse1 _ c hw hh)         = BBox (c .-^ v) (c .+^ v) 
    where v = V2 hw hh


instance Boundary (Picture u) where
  type BoundaryUnit (Picture u) = u       
  boundary Empty                = ZeroBB
  boundary (Single  (_,bb) _)   = bb
  boundary (Multi   (_,bb) _)   = bb
  boundary (Picture (_,bb) _ _) = bb
  boundary (Clip    (_,bb) _ _) = bb




--------------------------------------------------------------------------------

-- Default attributes

psBlack :: PSColour
psBlack = PSRgb 0 0 0
 
-- aka the standard frame
frameDefault :: Num u => Frame2 u 
frameDefault = ortho zeroPt




--------------------------------------------------------------------------------
-- Construction

empty :: Picture u
empty = Empty

-- | Lifts primitives to Pictures...
frame :: (Num u, Ord u) => Primitive u -> Picture u
frame p = Single (frameDefault, boundary p) p 

-- maybe we need a similar function with a bounding box so it 
-- can build text labels within some bound



multi :: (Num u, Ord u) => [Primitive u] -> Picture u
multi ps = Multi (frameDefault, mconcat $ map boundary ps) ps 



-- | Convert the list of vertices to a path of straight line 
-- segments.
vertexPath :: [Point2 u] -> Path u
vertexPath []     = error "straightLinePath - empty point list"
vertexPath (x:xs) = Path x (map PLine xs)




--------------------------------------------------------------------------------
-- Take Paths to Primitives


ostrokePath :: (Num u, Ord u) 
            => PSColour -> [StrokeAttr] -> Path u -> Primitive u
ostrokePath c attrs p = Path1 (c, OStroke attrs) p

cstrokePath :: (Num u, Ord u) 
            => PSColour -> [StrokeAttr] -> Path u -> Primitive u
cstrokePath c attrs p = Path1 (c, CStroke attrs) p

class Stroke t where
  ostroke :: (Num u, Ord u) => t -> Path u -> Primitive u
  cstroke :: (Num u, Ord u) => t -> Path u -> Primitive u

instance Stroke () where
  ostroke () = ostrokePath psBlack []
  cstroke () = cstrokePath psBlack []

instance Stroke PSColour where
  ostroke c = ostrokePath c []
  cstroke c = cstrokePath c []

instance Stroke (RGB3 Double) where
  ostroke c = ostrokePath (toPSColour c) []
  cstroke c = cstrokePath (toPSColour c) []

instance Stroke (HSB3 Double) where
  ostroke c = ostrokePath (toPSColour c) []
  cstroke c = cstrokePath (toPSColour c) []

instance Stroke (Gray Double) where
  ostroke c = ostrokePath (toPSColour c) []
  cstroke c = cstrokePath (toPSColour c) []


instance Stroke StrokeAttr where
  ostroke x = ostrokePath psBlack [x]
  cstroke x = cstrokePath psBlack [x]

instance Stroke [StrokeAttr] where
  ostroke xs = ostrokePath psBlack xs
  cstroke xs = cstrokePath psBlack xs


instance Stroke (PSColour,StrokeAttr) where
  ostroke (c,x) = ostrokePath c [x]
  cstroke (c,x) = cstrokePath c [x]

instance Stroke (RGB3 Double,StrokeAttr) where
  ostroke (c,x) = ostrokePath (toPSColour c) [x]
  cstroke (c,x) = cstrokePath (toPSColour c) [x]

instance Stroke (HSB3 Double,StrokeAttr) where
  ostroke (c,x) = ostrokePath (toPSColour c) [x]
  cstroke (c,x) = cstrokePath (toPSColour c) [x]

instance Stroke (Gray Double,StrokeAttr) where
  ostroke (c,x) = ostrokePath (toPSColour c) [x]
  cstroke (c,x) = cstrokePath (toPSColour c) [x]

instance Stroke (PSColour,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath c xs
  cstroke (c,xs) = cstrokePath c xs

instance Stroke (RGB3 Double,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath (toPSColour c) xs
  cstroke (c,xs) = cstrokePath (toPSColour c) xs

instance Stroke (HSB3 Double,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath (toPSColour c) xs
  cstroke (c,xs) = cstrokePath (toPSColour c) xs

instance Stroke (Gray Double,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath (toPSColour c) xs
  cstroke (c,xs) = cstrokePath (toPSColour c) xs


-- | Create an open stoke coloured black.
zostroke :: (Num u, Ord u) => Path u -> Primitive u
zostroke = ostrokePath psBlack []
 
-- | Create a closed stroke coloured black.
zcstroke :: (Num u, Ord u) => Path u -> Primitive u
zcstroke = cstrokePath psBlack []




-- fills only have one property - colour
-- Having a fill class seems uniform as we have a stroke class 



fillPath :: (Num u, Ord u) => PSColour -> Path u -> Primitive u
fillPath c p = Path1 (c,CFill) p

class Fill t where
  fill :: (Num u, Ord u) => t -> Path u -> Primitive u
 

instance Fill ()                where fill () = fillPath psBlack 
instance Fill PSColour          where fill = fillPath
instance Fill (RGB3 Double)     where fill = fillPath . toPSColour
instance Fill (HSB3 Double)     where fill = fillPath . toPSColour
instance Fill (Gray Double)     where fill = fillPath . toPSColour

-- | Create a filled path coloured black. 
zfill :: (Num u, Ord u) => Path u -> Primitive u
zfill = fillPath psBlack


--------------------------------------------------------------------------------
-- Labels to primitive

mkTextLabel :: PSColour -> FontAttr -> Point2 u -> String -> Primitive u
mkTextLabel c attr pt txt = Label1 (c,attr) (Label pt txt)

-- SVG seems to have an issue with /Courier/ and needs /Courier New/.

default_font :: FontAttr
default_font = FontAttr "Courier" "Courier New" 10

class TextLabel t where 
  textlabel :: t -> Point2 u -> String -> Primitive u


instance TextLabel () where textlabel () = mkTextLabel psBlack default_font

instance TextLabel PSColour where
  textlabel c = mkTextLabel c default_font

instance TextLabel (RGB3 Double) where
  textlabel c = mkTextLabel (toPSColour c) default_font

instance TextLabel (HSB3 Double) where
  textlabel c = mkTextLabel (toPSColour c) default_font

instance TextLabel (Gray Double) where
  textlabel c = mkTextLabel (toPSColour c) default_font

instance TextLabel FontAttr where
  textlabel a = mkTextLabel psBlack a

instance TextLabel (PSColour,FontAttr) where
  textlabel (c,a) = mkTextLabel c a

instance TextLabel (RGB3 Double,FontAttr) where
  textlabel (c,a) = mkTextLabel (toPSColour c) a

instance TextLabel (HSB3 Double,FontAttr) where
  textlabel (c,a) = mkTextLabel (toPSColour c) a

instance TextLabel (Gray Double,FontAttr) where
  textlabel (c,a) = mkTextLabel (toPSColour c) a

-- | Create a label where the font is @Courier@, text size is 10 
-- and colour is black.
ztextlabel :: Point2 u -> String -> Primitive u
ztextlabel = mkTextLabel psBlack default_font

--------------------------------------------------------------------------------

mkEllipse :: Num u => PSColour -> DrawEllipse -> Point2 u -> u -> u -> Primitive u
mkEllipse c dp pt hw hh = Ellipse1 (c,dp) pt hw hh


ellipseDefault :: EllipseProps
ellipseDefault = (psBlack, EFill)


-- | Instances will create a filled ellipse unless the supplied 
-- element /implies/ a stoked ellipse, e.g.:
--
-- > ellipse (LineWidth 4) zeroPt 40 40 
-- > ellipse EFill zeroPt 40 40  
--
class Ellipse t where
  ellipse :: Fractional u => t -> Point2 u -> u -> u -> Primitive u

instance Ellipse ()             where ellipse () = zellipse
instance Ellipse PSColour       where ellipse c  = mkEllipse c EFill
instance Ellipse DrawEllipse    where ellipse dp = mkEllipse psBlack dp

instance Ellipse StrokeAttr     where 
    ellipse = mkEllipse psBlack . EStroke . return

instance Ellipse [StrokeAttr]   where 
    ellipse = mkEllipse psBlack . EStroke

instance Ellipse (RGB3 Double) where 
    ellipse c = mkEllipse (toPSColour c) EFill

instance Ellipse (HSB3 Double) where 
    ellipse c = mkEllipse (toPSColour c) EFill

instance Ellipse (Gray Double) where 
    ellipse c = mkEllipse (toPSColour c) EFill


instance Ellipse (PSColour,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse c dp

instance Ellipse (RGB3 Double,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse (toPSColour c) dp

instance Ellipse (HSB3 Double,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse (toPSColour c) dp

instance Ellipse (Gray Double,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse (toPSColour c) dp


instance Ellipse (PSColour,StrokeAttr) where 
    ellipse (c,x) = mkEllipse c (EStroke [x])

instance Ellipse (RGB3 Double,StrokeAttr) where 
    ellipse (c,x) = mkEllipse (toPSColour c) (EStroke [x])

instance Ellipse (HSB3 Double,StrokeAttr) where 
    ellipse (c,x) = mkEllipse (toPSColour c) (EStroke [x])

instance Ellipse (Gray Double,StrokeAttr) where 
    ellipse (c,x) = mkEllipse (toPSColour c) (EStroke [x])

instance Ellipse (PSColour,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse c (EStroke xs)

instance Ellipse (RGB3 Double,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse (toPSColour c) (EStroke xs)

instance Ellipse (HSB3 Double,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse (toPSColour c) (EStroke xs)

instance Ellipse (Gray Double,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse (toPSColour c) (EStroke xs)


-- | Create a black, filled ellipse. 
zellipse :: Num u => Point2 u -> u -> u -> Primitive u
zellipse = uncurry mkEllipse ellipseDefault


--------------------------------------------------------------------------------

-- Operations on pictures and paths


nullPicture :: Picture u -> Bool
nullPicture Empty = True
nullPicture _     = False



extractFrame :: Num u => Picture u -> Frame2 u
extractFrame Empty                = ortho zeroPt
extractFrame (Single  (fr,_) _)   = fr
extractFrame (Multi   (fr,_) _)   = fr
extractFrame (Picture (fr,_) _ _) = fr
extractFrame (Clip    (fr,_) _ _) = fr



mapMeasure :: (Measure u -> Measure u) -> Picture u -> Picture u
mapMeasure _ Empty            = Empty
mapMeasure f (Single  m prim) = Single (f m) prim
mapMeasure f (Multi   m ps)   = Multi (f m) ps
mapMeasure f (Picture m a b)  = Picture (f m) a b
mapMeasure f (Clip    m x p)  = Clip (f m) x p


movePic :: Num u => Vec2 u -> Picture u -> Picture u
movePic v = mapMeasure (moveMeasure v) 

  
moveMeasure :: Num u => Vec2 u -> Measure u -> Measure u
moveMeasure v (fr,bb) = (displaceOrigin v fr, pointwise (.+^ v) bb) 



--------------------------------------------------------------------------------

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
  fn ZeroBB      = (ZeroBB, Nothing)
  fn bb@(BBox (P2 llx lly) (P2 urx ury))
      | llx < 4 || lly < 4  = (BBox ll ur, Just $ V2 x y)
      | otherwise           = (bb, Nothing)
    where 
      x  = 4 - llx
      y  = 4 - lly
      ll = P2 (llx+x) (lly+y)
      ur = P2 (urx+x) (ury+y)  


