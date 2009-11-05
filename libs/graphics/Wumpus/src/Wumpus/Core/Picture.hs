{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
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

module Wumpus.Core.Picture where

import Wumpus.Core.BoundingBox hiding ( center )
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.PictureLanguage

import Data.FunctionExtras
import Data.Groupoid

import Data.AffineSpace

import Text.PrettyPrint.Leijen

import Data.List                ( mapAccumR )
import Data.Monoid
import Data.Sequence            ( Seq )
import qualified Data.Sequence  as S



data Picture u = Empty
               | Single   (Measure u) (Primitive u)
               | Multi    (Measure u) [Primitive u] -- multiple prims in same affine frame
               | Picture  (Measure u) (Picture u) (Picture u)
               | Clip     (Measure u) (Path u)    (Picture u)
  deriving (Eq,Show) 


-- Ellipses are a primitive so they can be drawn efficiently.
-- 
-- Arcs are path primitives in PostScript, however following that
-- example for Wumpus is tricky. Arcs don't have the nice 
-- properties of bezier curves where affine transformations can 
-- just transform the control points and a bounding box can be 
-- considered the bounds of the control points.
-- 
-- So if we making arc a type of path (with curve and line 
-- segment) would create a lot of hard work. Instead we use
-- the PostScript command @arc@ only to create ellipses and
-- circles (ellipses are actually circles with non-uniform 
-- scaling).
--

data Primitive u = Path1    PathProps (Path u)
                 | Label1   LabelProps (Label u) 
                 | Ellipse1 { 
                      ellipseProps  :: EllipseProps,
                      ellipseCenter :: Point2 u,
                      ellipseWidth  :: u,
                      ellipseHeight :: u 
                    } 
  deriving (Eq,Show)


data Path u = Path DrawProp (Point2 u) [PathSeg u]
  deriving (Eq,Show)

type DPath = Path Double


data PathSeg u = PCurve  (Point2 u) (Point2 u) (Point2 u)
               | PLine   (Point2 u)
  deriving (Eq,Show)

type DPathSeg = PathSeg Double

data Label u = Label (Point2 u) String
  deriving (Eq,Show)

type DLabel = Label Double


type PathProps    = (PSColour, Seq PenAttr)
type LabelProps   = (PSColour, FontAttr)
type EllipseProps = (PSColour, DrawProp)

-- Measure = (_current_ frame x bounding box)

type Measure u = (Frame2 u, BoundingBox u) 

type DMeasure = Measure Double


-- | Note when drawn /filled/ and drawn /stroked/ the same 
-- polygon will have (slightly) different size: 
-- 
-- * A filled shape fills /within/ the boundary of the shape
-- 
-- * A stroked shape draws a pen line around the boundary 
--   of the shape. The actual size depends on the thickness
--   of the line (stroke width).
--
data DrawProp = OStroke | CStroke | CFill
  deriving (Eq,Show)




--------------------------------------------------------------------------------
-- Pretty printing

instance (Num u, Pretty u) => Pretty (Picture u) where
  pretty Empty              = text "*empty*"
  pretty (Single m prim)    = ppMeasure m <$> indent 2 (pretty prim)

  pretty (Multi m prims)    = ppMeasure m <$> indent 2 (list $ map pretty prims)
  pretty (Picture m pl pr)  = 
      ppMeasure m <$> indent 2 (text "LEFT" <+> pretty pl)
                  <$> indent 2 (text "RGHT" <+> pretty pr)

  pretty (Clip m path p)    = 
      text "Clip:" <+> ppMeasure m <$> indent 2 (pretty path)
                                   <$> indent 2 (pretty p)

ppMeasure :: (Num u, Pretty u) => Measure u -> Doc
ppMeasure (fr,bbox) = align (ppfr <$> pretty bbox) where
   ppfr = if standardFrame fr then text "*std-frame*" else pretty fr


instance Pretty u => Pretty (Primitive u) where
  pretty (Path1 _ path)     = pretty "path:" <+> pretty path
  pretty (Label1 _ lbl)     = pretty lbl
  pretty (Ellipse1 _ c w h) = pretty "ellipse" <+> pretty c
                                               <+> text "w:" <> pretty w
                                               <+> text "h:" <> pretty h


instance Pretty u => Pretty (Path u) where
   pretty (Path _ pt ps) = pretty pt <> hcat (map pretty ps)

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
  Path dp st xs `gappend` Path _ st' xs' = Path dp st (xs ++ (PLine st' : xs'))


instance Pointwise (Path u) where
  type Pt (Path u) = Point2 u
  pointwise f (Path dp st xs) = Path dp (f st) (map (pointwise f) xs)

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

-- TO DO
-- What should bounds be of an empty picture?

instance (Num u, Ord u) => Horizontal (Picture u) where
  type HUnit (Picture u) = u

  moveH a    = movePic (hvec a) 
  leftBound  = maybe 0 id . leftPlane . extractBounds
  rightBound = maybe 0 id . rightPlane . extractBounds

instance (Num u, Ord u) => Vertical (Picture u) where
  type VUnit (Picture u) = u

  moveV a     = movePic (vvec a) 
  topBound    = maybe 0 id . upperPlane . extractBounds
  bottomBound = maybe 0 id . lowerPlane . extractBounds

instance (Num u, Ord u) => Composite (Picture u) where
  cempty  = picEmpty

  a     `composite` Empty = a
  Empty `composite` b     = b
  a     `composite` b     = Picture (frameDefault, bb) a b where
                            bb = union (extractBounds a) (extractBounds b)



instance (Num u, Ord u, Horizontal (Picture u), Vertical (Picture u),
          HUnit (Picture u) ~ VUnit (Picture u)) => 
      PMove (Picture u) where
  pmove x y = movePic (V2 x y)


--------------------------------------------------------------------------------

-- Default attributes

-- aka the standard frame
psBlack :: PSColour
psBlack = PSRgb 0 0 0
 
frameDefault :: Num u => Frame2 u 
frameDefault = ortho zeroPt

pathDefault :: PathProps 
pathDefault = (psBlack, mempty)

-- Firefox seems to have a issues with Courier.

labelDefault :: LabelProps
labelDefault = (psBlack, FontAttr "Courier" "Courier New" 10)

ellipseDefault :: EllipseProps
ellipseDefault = (psBlack, CFill)


--------------------------------------------------------------------------------
-- Construction

-- The code here is ugly and needs thought...

picEmpty :: Picture u
picEmpty = Empty




straightLinePath :: DrawProp -> [Point2 u] -> Path u
straightLinePath _     []     = error "straightLinePath - empty point list"
straightLinePath props (x:xs) = Path props x (map PLine xs)



picPath :: (Num u, Ord u) => Path u -> Picture u
picPath p = Single (frameDefault, pathBounds p) (Path1 pathDefault p)

picMultiPath :: (Num u, Ord u) => [Path u] -> Picture u
picMultiPath ps = Multi (frameDefault, mconcat (map pathBounds ps)) (map f ps)
  where f = Path1 pathDefault

-- The width guesses by picLabel1 and picLabel are very poor...

picLabel1 :: (Num u, Ord u) => Int -> String -> Picture u
picLabel1 fontsz str = Single (frameDefault, bb) lbl where
  bb  = BBox zeroPt (P2 w (fromIntegral fontsz))
  w   = fromIntegral $ fontsz * length str
  lbl = Label1 labelDefault (Label zeroPt str) 


picLabel :: forall u. (Num u, Ord u) => Int -> Int -> String -> Picture u
picLabel fontsz linespace str = Multi (frameDefault, bb) lbls where
  xs   = lines str
  lc   = length xs
  w    = fromIntegral $ fontsz * (maximum . map length) xs
  h    = fromIntegral $ fontsz * lc + linespace*(lc-1)
  bb   = BBox zeroPt (P2 w h)
  lbls = snd $ mapAccumR fn zeroPt xs
  fn pt ss = let pt' = pt .+^ (V2 (0::u) (fromIntegral $ fontsz + linespace))
             in (pt', Label1 labelDefault (Label pt ss))

picEllipse :: Num u => EllipseProps -> u -> u -> Picture u
picEllipse dp w h = Single (frameDefault,bb) ellp where
    v    = V2 w h
    bb   = BBox (zeroPt .-^ v) (zeroPt .+^ v)
    ellp = Ellipse1 dp zeroPt w h




--------------------------------------------------------------------------------

-- Operations on pictures and paths


nullPicture :: Picture u -> Bool
nullPicture Empty = True
nullPicture _     = False




pathBounds :: (Num u, Ord u) => Path u -> BoundingBox u
pathBounds (Path _ st xs) = trace $ st : foldr f [] xs
  where
    f (PLine p1)        acc  = p1 : acc
    f (PCurve p1 p2 p3) acc  = p1 : p2 : p3 : acc 


extractBounds :: (Num u, Ord u) => Picture u -> BoundingBox u
extractBounds Empty                = ZeroBB
extractBounds (Single  (_,bb) _)   = bb
extractBounds (Multi   (_,bb) _)   = bb
extractBounds (Picture (_,bb) _ _) = bb
extractBounds (Clip    (_,bb) _ _) = bb

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

translateBBox :: BoundingBox Double 
              -> (Maybe (Double,Double), BoundingBox Double)
translateBBox ZeroBB      = (Nothing,ZeroBB)
translateBBox bb@(BBox (P2 llx lly) (P2 urx ury))
    | llx < 4 || lly < 4  = (Just (x,y), BBox ll ur)            
    | otherwise           = (Nothing, bb)
  where 
     x  = 4 - llx
     y  = 4 - lly
     ll = P2 (llx+x) (lly+y)
     ur = P2 (urx+x) (ury+y)  

