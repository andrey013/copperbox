{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.BoundingBox
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Bounding box that supports 'mempty'.
--
--------------------------------------------------------------------------------

module Wumpus.Core.BoundingBox 
  ( 
  -- * Types
    BoundingBox(..)
  , DBoundingBox
  , CardinalPoint(..)

  -- * Type class
  , Boundary(..)
  
  -- * Operations
  , bbox
  , union 
  , trace
  , corners
  , lowerLeftUpperRight
  , withinBB
  , boundaryWidth
  , boundaryHeight
  , boundaryBottomLeft
  , boundaryTopRight
  , boundaryTopLeft
  , boundaryBottomRight
  , boundaryPoint
  , leftPlane
  , rightPlane
  , lowerPlane
  , upperPlane

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.Geometry
import Wumpus.Core.Utils ( CMinMax(..), within )

import Text.PrettyPrint.Leijen hiding ( width )

import Data.Monoid


-- | Bounding box of a picture.
-- 
-- We want to represent empty pictures and their concatentation:
-- 
-- > empty ++ empty = empty
-- > empty ++ pic1  = pic1
-- 
-- Therefore the bounding box representation is obliged to 
-- support the bounds of an empty picture. One /trick/ is to 
-- place represent the empty bounding box with lower left corner
-- at +infinity and upper-right corner at -infinity. Thus the 
-- union operation which finds the component-wise min of the 
-- lowerleft coordinates and component-wise max of the 
-- upper-right works. However this only works for points with
-- a unit type supporting infinity.
-- 
data BoundingBox a = ZeroBB 
                   | BBox { 
                         ll_corner :: Point2 a, 
                         ur_corner :: Point2 a 
                       }
  deriving (Eq,Show)

type DBoundingBox = BoundingBox Double

data CardinalPoint = C | N | NE | E | SE | S | SW | W | NW
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- instances

-- BBox has been augmented with the special ZeroBB case to enable
-- monoidal operations...

instance Ord a => Monoid (BoundingBox a) where
  mempty  = ZeroBB
  mappend = union


instance Pretty a => Pretty (BoundingBox a) where
  pretty ZeroBB       = text "|_ +inf -inf _|"
  pretty (BBox p0 p1) = text "|_" <+> pretty p0 <+> pretty p1 <+> text "_|" 


--------------------------------------------------------------------------------
-- 

type instance AUnit (BoundingBox u) = u

instance (Num u, Ord u) => Scale (BoundingBox u) where
  scale _ _ ZeroBB = ZeroBB
  scale x y bb     = trace $ map (scale x y) $ corners bb



--------------------------------------------------------------------------------
-- Boundary class

class Boundary a where
  type BoundaryUnit a
  boundary :: a -> BoundingBox (BoundaryUnit a)


--------------------------------------------------------------------------------


instance Pointwise (BoundingBox a) where
  type Pt (BoundingBox a) = Point2 a
  pointwise _ ZeroBB       = ZeroBB
  pointwise f (BBox bl tr) = BBox (f bl) (f tr)


--------------------------------------------------------------------------------

bbox :: Point2 a -> Point2 a -> BoundingBox a
bbox = BBox 

union :: Ord a => BoundingBox a -> BoundingBox a -> BoundingBox a
ZeroBB     `union` b            = b
a          `union` ZeroBB       = a
BBox ll ur `union` BBox ll' ur' = BBox (cmin ll ll') (cmax ur ur')

-- Trace the point list finding the /extremity/...

trace :: (Num a, Ord a) => [Point2 a] -> BoundingBox a
trace []     = ZeroBB
trace (p:ps) = uncurry BBox $ foldr (\z (a,b) -> (cmin z a, cmax z b) ) (p,p) ps

-- TO CHECK:
-- Corners might be a problem with the introduction of ZeroBB...

corners :: BoundingBox a -> [Point2 a]
corners ZeroBB                             = []   
corners (BBox bl@(P2 x0 y0) tr@(P2 x1 y1)) = [bl, br, tr, tl] where
    br = P2 x1 y0
    tl = P2 x0 y1


lowerLeftUpperRight :: (a,a,a,a) -> BoundingBox a -> (a,a,a,a)
lowerLeftUpperRight dflt ZeroBB                      = dflt
lowerLeftUpperRight _   (BBox (P2 x0 y0) (P2 x1 y1)) = (x0,y0,x1,y1)



withinBB :: Ord a => Point2 a -> BoundingBox a -> Bool
withinBB _ ZeroBB       = False
withinBB p (BBox ll ur) = within p ll ur


boundaryWidth :: Num a => BoundingBox a -> a
boundaryWidth ZeroBB                         = 0
boundaryWidth (BBox (P2 xmin _) (P2 xmax _)) = xmax - xmin

boundaryHeight :: Num a => BoundingBox a -> a
boundaryHeight ZeroBB                         = 0
boundaryHeight (BBox (P2 _ ymin) (P2 _ ymax)) = ymax - ymin


--------------------------------------------------------------------------------

-- Points on the boundary
-- These types become horrible with the introduction of ZeroBB


boundaryBottomLeft  :: BoundingBox a -> Maybe (Point2 a)
boundaryBottomLeft  = mkPoint (\x y _ _-> P2 x y)

boundaryTopRight    :: BoundingBox a -> Maybe (Point2 a)
boundaryTopRight    = mkPoint (\_ _ x y -> P2 x y)

boundaryTopLeft     :: BoundingBox a -> Maybe (Point2 a)
boundaryTopLeft     = mkPoint (\x _ _ y -> P2 x y)

boundaryBottomRight :: BoundingBox a -> Maybe (Point2 a)
boundaryBottomRight = mkPoint (\_ y x _ -> P2 x y)

-- convoluted by avoids the Fractional obligation of boundaryPoint
mkPoint :: (a -> a -> a -> a -> Point2 a) -> BoundingBox a -> Maybe (Point2 a)
mkPoint _  ZeroBB = Nothing
mkPoint fn (BBox (P2 x0 y0) (P2 x1 y1)) = Just $ fn x0 y0 x1 y1


boundaryPoint :: Fractional a 
              => CardinalPoint -> BoundingBox a -> Maybe (Point2 a)
boundaryPoint _   ZeroBB                       = Nothing
boundaryPoint loc (BBox (P2 x0 y0) (P2 x1 y1)) = Just $ fn loc where
  fn C      = P2 xMid   yMid
  fn N      = P2 xMid   y1
  fn NE     = P2 x1     y1
  fn E      = P2 x1     yMid
  fn SE     = P2 x1     y0
  fn S      = P2 xMid   y0
  fn SW     = P2 x0     y0
  fn W      = P2 x0     yMid
  fn NW     = P2 x0     y1       

  xMid      = x0 + 0.5 * (x1 - x0)
  yMid      = y0 + 0.5 * (y1 - y0)


--------------------------------------------------------------------------------

-- /planes/ on the bounding box

leftPlane :: BoundingBox a -> Maybe a
leftPlane ZeroBB            = Nothing
leftPlane (BBox (P2 l _) _) = Just l

rightPlane :: BoundingBox a -> Maybe a
rightPlane ZeroBB            = Nothing
rightPlane (BBox _ (P2 r _)) = Just r

lowerPlane :: BoundingBox a -> Maybe a
lowerPlane ZeroBB            = Nothing
lowerPlane (BBox (P2 _ l) _) = Just l

upperPlane :: BoundingBox a -> Maybe a
upperPlane ZeroBB            = Nothing
upperPlane (BBox _ (P2 _ u)) = Just u






