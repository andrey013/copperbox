{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PictureInternal
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
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
  , PrimCTM

  , PathProps                   -- hide in Wumpus.Core export?
  , LabelProps                  -- hide in Wumpus.Core export?
  , EllipseProps                -- 
  , DrawPath(..)                -- hide in Wumpus.Core export?
  , DrawEllipse(..)
  , Locale  
 
  -- * Type class

  , PSUnit(..)

  -- * Debug printing
  , printPicture

  -- * Transformations on Primitives
  , translatePrimitive
  , rotatePrimitive
  , scalePrimitive
  , uniformScalePrimitive

  -- * PrimCTM
  , identityCTM
  , scaleCTM
  , matrixRepCTM
  , translMatrixRepCTM
  
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
import Wumpus.Core.PtSize
import Wumpus.Core.TextEncodingInternal
import Wumpus.Core.Utils hiding ( parens )


import Data.AffineSpace
import Data.Semigroup

import Text.PrettyPrint.Leijen          -- package: wl-pprint

import qualified Data.Foldable as F

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
-- > Leaf (aka Single) | Picture (OneList tree)
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
               | Leaf     (Locale u)          (Primitive u)
               | Picture  (Locale u)          (OneList (Picture u))
               | Clip     (Locale u) (Path u) (Picture u)
  deriving (Eq,Show) 

-- Note - Single is rather inefficient in hindsight.
-- 
-- A list of primitives would be better, and Leaf is probably a 
-- better name.
--



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
-- Wumpus does not follow PostScript employing arc as a general 
-- path primitive - arcs are used only to draw ellipses. This 
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
      , label_ctm         :: PrimCTM u
      }
  deriving (Eq,Show)

type DLabel = Label Double

-- Ellipse represented by center and half_width * half_height
--
data PrimEllipse u = PrimEllipse 
      { ellipse_center        :: Point2 u
      , ellipse_half_width    :: u
      , ellipse_half_height   :: u 
      , ellipse_ctm           :: PrimCTM u
      } 
  deriving (Eq,Show)

type DPrimEllipse = PrimEllipse Double




data PrimCTM u = PrimCTM 
      { ctm_scale_x     :: u
      , ctm_scale_y     :: u
      , ctm_rotation    :: Radian 
      }
  deriving (Eq,Show)


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

printPicture :: (Num u, PSUnit u) => Picture u -> IO ()
printPicture pic = putDoc (pretty pic) >> putStrLn []


instance (Num u, PSUnit u) => Pretty (Picture u) where
  pretty (PicBlank m)       = 
      text "** Blank-picture **"  <+> ppLocale m

  pretty (Leaf m prim)      = 
      text "** Leaf-picture **"   <$> ppLocale m <$> indent 2 (pretty prim)

  pretty (Picture m ones)   = 
      text "** Multi-picture **"  <$> ppLocale m 
                                  <$> indent 2 (ppPics ones)

  pretty (Clip m cpath p)   = 
      text "** Clip-path **" <+> ppLocale m 
                             <$> indent 2 (pretty cpath <$> pretty p)


ppPics :: (Num u, PSUnit u) => OneList (Picture u) -> Doc
ppPics ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, acc <$> text "-- leaf" <+> int n <$> pretty e <> line)

ppLocale :: (Num u, PSUnit u) => Locale u -> Doc
ppLocale (fr,bb) = ppfr <$> pretty bb where
   ppfr = if standardFrame fr then text "* std-frame *" else pretty fr


instance PSUnit u => Pretty (Primitive u) where
  pretty (PPath attr p)     = 
      text "path:"      <$> indent 2 (ppPathProps    attr <$> pretty p)

  pretty (PLabel attr l)  = 
      text "label:"     <$> indent 2 (ppLabelProps   attr <$> pretty l)

  pretty (PEllipse attr e)  = 
      text "ellipse:"   <$> indent 2 (ppEllipseProps attr <$> pretty e)


instance PSUnit u => Pretty (Path u) where
   pretty (Path pt ps) = vcat (start  : map pretty ps) 
      where
        start = fill 12 (text "start_point") <> pretty pt

instance PSUnit u => Pretty (PathSegment u) where
  pretty (PCurveTo p1 p2 p3)  = 
    fill 12 (text "curve_to") <> pretty p1 <+> pretty p2 <+> pretty p3

  pretty (PLineTo pt)         = fill 12 (text "line_to") <> pretty pt

instance PSUnit u => Pretty (Label u) where
  pretty (Label pt s ctm) = 
     dquotes (pretty s) <$> text "baseline_left=" <> pretty pt
                        <+> text "ctm="           <> pretty ctm

instance PSUnit u => Pretty (PrimEllipse u) where
  pretty (PrimEllipse ctr w h ctm) = pretty "center=" <> pretty ctr
                                 <+> text "w="        <> dtruncPP w
                                 <+> text "h="        <> dtruncPP h
                                 <+> text "ctm="      <> pretty ctm
  

instance PSUnit u => Pretty (PrimCTM u) where
  pretty (PrimCTM x y ang) = 
      parens (text "CTM" <+> dtruncPP x <+> dtruncPP y <+> pretty ang)


ppPathProps :: PathProps -> Doc
ppPathProps (rgb, CFill)       = pretty rgb <+> text "Fill"
ppPathProps (rgb, (CStroke _)) = pretty rgb <+> text "Closed-stroke"
ppPathProps (rgb, _)           = pretty rgb <+> text "Open-stroke"

ppLabelProps :: LabelProps -> Doc
ppLabelProps (rgb, attr) = pretty rgb <+> text (font_name $ font_face attr)

ppEllipseProps :: EllipseProps -> Doc
ppEllipseProps (rgb, EFill) = pretty rgb <+> text "Fill"
ppEllipseProps (rgb, _)     = pretty rgb <+> text "Stroke"


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
  transform ctm pic = trafoPicture (transform ctm) (transform ctm) pic


instance (Floating u, Real u) => Rotate (Picture u) where
  rotate = rotatePicture 

instance (Floating u, Real u) => RotateAbout (Picture u) where
  rotateAbout = rotatePictureAbout

instance (Num u, Ord u) => Scale (Picture u) where
  scale = scalePicture

instance (Num u, Ord u) => Translate (Picture u) where
  translate = translatePicture




-- Primitives


-- | Rotate a Primitive.
-- 
-- Note - this is not an affine transformation as Primitives are
-- not regarded as being in an affine frame.
--
-- * Paths are rotated about their start point.
--
-- * Labels are rotated about the bottom-left corner.
--
-- * Ellipses are rotated about the center.
--
-- For Primitives and Ellipses applying a rotation and or a scale 
-- will generate an additional matrix transformation in the 
-- generated PostScript. For Paths all transformations are
-- \"cost-free\".
--
rotatePrimitive :: (Real u, Floating u) 
                => Radian -> Primitive u -> Primitive u
rotatePrimitive ang (PPath    a path) = PPath    a $ rotatePath ang path
rotatePrimitive ang (PLabel   a lbl)  = PLabel   a $ rotateLabel ang lbl
rotatePrimitive ang (PEllipse a ell)  = PEllipse a $ rotateEllipse ang ell


-- | Scale a Primitive.
-- 
-- Note - this is not an affine transformation as Primitives are
-- not regarded as being in an affine frame.
--
-- An affine scaling uniformly scales all the elements in a 
-- Picture. It is just a change of the Picture\'s basis vectors.
-- The elements within the Picture are unchanged - though 
-- obviously rendering changes according to the transformation.
--
-- By contrast, the scaling operation on Primitives changes the 
-- properties of the object as it is applied - e.g. for a path
-- the vector between the start point and all subsequent points
-- is changed with respect to the x,y scaling factors; for an
-- ellipse the half-width and half-height of the ellipse is
-- scaled.
--
-- For Primitives and Ellipses applying a rotation and or a scale 
-- will generate an additional matrix transformation in the 
-- generated PostScript. For Paths all transformations are 
-- \"cost-free\".
--
scalePrimitive :: Num u => u -> u -> Primitive u -> Primitive u
scalePrimitive x y (PPath    a path) = PPath    a $ scalePath x y path
scalePrimitive x y (PLabel   a lbl)  = PLabel   a $ scaleLabel x y lbl
scalePrimitive x y (PEllipse a ell)  = PEllipse a $ scaleEllipse x y ell

-- | Apply a uniform scale to a Primitive.
--
uniformScalePrimitive :: Num u => u -> Primitive u -> Primitive u
uniformScalePrimitive d = scalePrimitive d d 

-- | Translate a primitive.
--
-- Translation is essentially \"cost-free\" for the generated 
-- PostScript or SVG. Paths are translated before the PostScript 
-- is generated. For Ellipses and Labels, translation will 
-- either move the bottom-left origin (Label) or center 
-- (Ellipse); or if they are also scaled or rotated the 
-- translation will be concatenated into the matrix operation in 
-- the generated output. 
-- 
translatePrimitive :: Num u => u -> u -> Primitive u -> Primitive u
translatePrimitive x y (PPath    a path) = PPath a $ translatePath x y path
translatePrimitive x y (PLabel   a lbl)  = PLabel a $ translateLabel x y lbl
translatePrimitive x y (PEllipse a ell)  = PEllipse a $ translateEllipse x y ell


--------------------------------------------------------------------------------

-- Helpers for the affine transformations

rotatePicture :: (Real u, Floating u) => Radian -> Picture u -> Picture u
rotatePicture ang = trafoPicture (rotate ang) (rotate ang)

rotatePictureAbout :: (Real u, Floating u) 
                   => Radian -> Point2 u -> Picture u -> Picture u
rotatePictureAbout ang pt = 
    trafoPicture (rotateAbout ang pt) (rotateAbout ang pt)
  
scalePicture :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
scalePicture x y = trafoPicture (scale x y) (scale x y)

translatePicture :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
translatePicture x y = trafoPicture (translate x y) (translate x y)

-- TODO - the nameing for these functions is confusing now that
-- I've added a Transform typeclass.
--
-- Look to unifying the naming scheme in someway.
--
trafoPicture :: (Num u, Ord u) 
             => (Point2 u -> Point2 u) 
             -> (Vec2 u -> Vec2 u) 
             -> Picture u 
             -> Picture u
trafoPicture fp fv = 
    mapLocale $ \(frm,bb) -> (trafoFrame fp fv frm, trafoBBox fp bb)



trafoFrame :: Num u
           => (Point2 u -> Point2 u) 
           -> (Vec2 u -> Vec2 u) 
           -> Frame2 u 
           -> Frame2 u
trafoFrame fp fv (Frame2 e0 e1 o) = Frame2 (fv e0) (fv e1) (fp o)


-- Bounding boxes need recalculating after a transformation.
-- For instance after a reflection in the y-axis br becomes bl.
trafoBBox :: (Num u, Ord u)
              => (Point2 u -> Point2 u) -> BoundingBox u -> BoundingBox u
trafoBBox fp bb = traceBoundary $ map fp $ [bl,br,tl,tr]
  where
    (bl,br,tr,tl) = corners bb


--------------------------------------------------------------------------------
-- Paths

-- Cannot support general matrix transform or rotateAbout on 
-- Ellipses or Labels so there are not supported on Paths.
--

-- rotatePath - rotate the path about its start point.
-- 
-- This is a visually intuitive interpretation - Primitives are
-- not in an affine space (they have an origin, i.e. the location 
-- (0,0), but don\'t not basis vectors) so manipulating them 
-- cannot follow the standard affine interpretation.
-- 
rotatePath :: (Real u, Floating u) => Radian -> Path u -> Path u
rotatePath ang (Path start xs) = 
    Path start $ map (pointwise (rotateAbout ang start)) xs

-- scalePath - scale the vector between each point and the start 
-- point.
--
-- This produces visually inituitive results. As primitives 
-- don\'t exist in an affine space / affine frame until they
-- are lifted to Pictures their manipulation cannot correspond
-- to the standard affine manipulations.
--
scalePath :: Num u => u -> u -> Path u -> Path u
scalePath x y (Path pt xs) = Path pt (map (pointwise fn) xs) 
  where
    fn p1 = let dif = p1 .-. pt in pt .+^ (scale x y $ dif)

-- translatePath - move all points in the path by the supplied 
-- x and y values.
--
translatePath :: Num u => u -> u -> Path u -> Path u
translatePath x y = pointwise (translate x y)



--------------------------------------------------------------------------------
-- Manipulating the Primitive CTM

identityCTM :: Num u => PrimCTM u
identityCTM = PrimCTM { ctm_scale_x = 1, ctm_scale_y = 1, ctm_rotation = 0 }

scaleCTM :: Num u => u -> u -> PrimCTM u -> PrimCTM u
scaleCTM x1 y1 (PrimCTM x y ang) = PrimCTM (x1*x) (y1*y) ang

rotateCTM :: Radian -> PrimCTM u -> PrimCTM u
rotateCTM ang1 (PrimCTM x y ang) = PrimCTM x y (circularModulo $ ang1+ang)

matrixRepCTM :: (Floating u, Real u) => PrimCTM u -> Matrix3'3 u
matrixRepCTM (PrimCTM x y ang) = 
    rotationMatrix (circularModulo ang) * scalingMatrix x y


-- Note - the order of combining a translation (i.e. the 
-- location of a point) and the CTM is crucial as matrix
-- multiplication is not commutative.
--
-- The function encapsulated the correct order.
--
translMatrixRepCTM :: (Floating u, Real u) 
                   => u -> u -> PrimCTM u -> Matrix3'3 u
translMatrixRepCTM x y ctm = translationMatrix x y * matrixRepCTM ctm

--------------------------------------------------------------------------------
-- Labels



-- Rotations on a (primitive) Label are interpreted as rotating
-- about the bottom-left corner.
--
rotateLabel :: Radian -> Label u -> Label u
rotateLabel ang (Label pt txt ctm) = Label pt txt (rotateCTM ang ctm)

scaleLabel :: Num u => u -> u -> Label u -> Label u
scaleLabel x y (Label pt txt ctm) = Label pt txt (scaleCTM x y ctm)


-- Change the bottom-left corner.
--
translateLabel :: Num u => u -> u -> Label u -> Label u
translateLabel x y (Label pt txt ctm) = Label (translate x y pt) txt ctm

--------------------------------------------------------------------------------
-- Ellipse


rotateEllipse :: Radian -> PrimEllipse u -> PrimEllipse u
rotateEllipse ang (PrimEllipse pt hw hh ctm) = 
    PrimEllipse pt hw hh (rotateCTM ang ctm)
    


scaleEllipse :: Num u => u -> u -> PrimEllipse u -> PrimEllipse u
scaleEllipse x y (PrimEllipse pt hw hh ctm) = 
    PrimEllipse (translate x y pt) hw hh (scaleCTM x y ctm)
    


-- Change the point
--
translateEllipse :: Num u => u -> u -> PrimEllipse u -> PrimEllipse u
translateEllipse x y (PrimEllipse pt hw hh ctm) = 
    PrimEllipse (translate x y pt) hw hh ctm
    

--------------------------------------------------------------------------------
-- Boundary

instance Boundary (Picture u) where
  boundary (PicBlank (_,bb))     = bb
  boundary (Leaf     (_,bb) _)   = bb
  boundary (Picture  (_,bb) _)   = bb
  boundary (Clip     (_,bb) _ _) = bb

instance (Num u, Ord u) => Boundary (Path u) where
  boundary (Path st xs) = traceBoundary $ st : foldr f [] xs where
      f (PLineTo p1)        acc  = p1 : acc
      f (PCurveTo p1 p2 p3) acc  = p1 : p2 : p3 : acc 


-- Note - this will calculate an approximate bounding box for 
-- text.

instance (Real u, Floating u, FromPtSize u) => Boundary (Primitive u) where
  boundary (PPath _ p)        = boundary p
  boundary (PLabel (_,a) l)   = primLabelBoundary a l 
  boundary (PEllipse _ e)     = boundary e



primLabelBoundary :: (Floating u, Real u, FromPtSize u) 
                  => FontAttr -> Label u -> BoundingBox u
primLabelBoundary attr (Label (P2 x y) xs ctm) = 
    retraceBoundary  (disp . (m33 *#)) untraf_bbox
  where
    disp        = (.+^ V2 x y)
    m33         = matrixRepCTM ctm
    untraf_bbox = textBounds (font_size attr) zeroPt char_count
    char_count  = textLength xs

instance (Real u, Floating u) => Boundary (PrimEllipse u) where
  boundary = ellipseBoundary

-- Find the bbox of an ellipse by drawing it as four bezier 
-- curves then trace all the points and control points to find
-- the bbox.
-- 
-- Note all_points takes three of the four points to avoid 
-- duplicating
-- /matched/ start-end points
--

ellipseBoundary :: (Real u, Floating u) => PrimEllipse u -> BoundingBox u
ellipseBoundary = traceBoundary . ellipseControlPoints

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
mapLocale f (Leaf     m prim) = Leaf (f m) prim
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
extractFrame (Leaf     (fr,_) _)   = fr
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
repositionProperties :: (Num u, Ord u) 
                     => Picture u -> (BoundingBox u, Maybe (Vec2 u))
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



