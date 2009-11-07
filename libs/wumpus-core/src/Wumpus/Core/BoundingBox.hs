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

  -- * Type class
  , Boundary(..)
  
  -- * Operations
  , union 
  , trace
  , corners
  , lowerLeftUpperRight
  , withinBB
  , width
  , height
  , bottomLeft
  , topRight
  , topLeft
  , bottomRight
  , center
  , north
  , south
  , east
  , west
  , northEast
  , southEast
  , southWest
  , northWest
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
                   | BBox { llPoint :: Point2 a, urPoint :: Point2 a }
  deriving (Eq,Show)

type DBoundingBox = BoundingBox Double


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

instance (Num u, Ord u) => Scale (BoundingBox u) where
  type ScaleUnit (BoundingBox u) = u
  scale _ _ ZeroBB = ZeroBB
  scale x y bb     = trace $ map (scale x y) $ corners bb



--------------------------------------------------------------------------------
-- Boundary class

class Boundary a where
  type BoundaryUnit a
  boundary :: a -> BoundingBox (BoundaryUnit a)


--------------------------------------------------------------------------------


union :: Ord a => BoundingBox a -> BoundingBox a -> BoundingBox a
ZeroBB     `union` b            = b
a          `union` ZeroBB       = a
BBox ll ur `union` BBox ll' ur' = BBox (cmin ll ll') (cmax ur ur')

instance Pointwise (BoundingBox a) where
  type Pt (BoundingBox a) = Point2 a
  pointwise _ ZeroBB       = ZeroBB
  pointwise f (BBox bl tr) = BBox (f bl) (f tr)


--------------------------------------------------------------------------------


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


width :: Num a => BoundingBox a -> a
width ZeroBB                         = 0
width (BBox (P2 xmin _) (P2 xmax _)) = xmax - xmin

height :: Num a => BoundingBox a -> a
height ZeroBB                         = 0
height (BBox (P2 _ ymin) (P2 _ ymax)) = ymax - ymin


--------------------------------------------------------------------------------

-- points on the boundary
-- These types become horrible with the introduction of ZeroBB


bottomLeft :: BoundingBox a -> Maybe (Point2 a)
bottomLeft ZeroBB      = Nothing
bottomLeft (BBox ll _) = Just ll

topRight :: BoundingBox a -> Maybe (Point2 a)
topRight ZeroBB      = Nothing
topRight (BBox _ ur) = Just ur

topLeft :: BoundingBox a -> Maybe (Point2 a)
topLeft ZeroBB                         = Nothing
topLeft (BBox (P2 xmin _) (P2 _ ymax)) = Just $ P2 xmin ymax

bottomRight :: BoundingBox a -> Maybe (Point2 a)
bottomRight ZeroBB                         = Nothing
bottomRight (BBox (P2 _ ymin) (P2 xmax _)) = Just $ P2 xmax ymin

center :: Fractional a => BoundingBox a -> Maybe (Point2 a)
center ZeroBB                     = Nothing
center (BBox (P2 x y) (P2 x' y')) = Just $ P2 (x+0.5*(x'-x)) (y+0.5*(y'-x))

north :: Fractional a => BoundingBox a -> Maybe (Point2 a)
north ZeroBB                            = Nothing
north (BBox (P2 xmin _) (P2 xmax ymax)) = Just $ P2 (xmin + 0.5*(xmax-xmin)) ymax

south :: Fractional a => BoundingBox a -> Maybe (Point2 a)
south ZeroBB                            = Nothing
south (BBox (P2 xmin ymin) (P2 xmax _)) = Just $ P2 (xmin + 0.5*(xmax-xmin)) ymin

east :: Fractional a => BoundingBox a -> Maybe (Point2 a)
east ZeroBB                            = Nothing
east (BBox (P2 _ ymin) (P2 xmax ymax)) = Just $ P2 xmax (ymin + 0.5*(ymax-ymin))

west :: Fractional a => BoundingBox a -> Maybe (Point2 a)
west ZeroBB                            = Nothing
west (BBox (P2 xmin ymin) (P2 _ ymax)) = Just $ P2 xmin (ymin + 0.5*(ymax-ymin))

northEast :: BoundingBox a -> Maybe (Point2 a)
southEast :: BoundingBox a -> Maybe (Point2 a)
southWest :: BoundingBox a -> Maybe (Point2 a)
northWest :: BoundingBox a -> Maybe (Point2 a)

northEast = topRight
southEast = bottomRight
southWest = bottomLeft
northWest = topLeft 

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






