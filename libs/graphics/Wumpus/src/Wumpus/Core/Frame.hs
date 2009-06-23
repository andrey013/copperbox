{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Frame
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Frames
--
--------------------------------------------------------------------------------


module Wumpus.Core.Frame where

import Wumpus.Core.Instances ()
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.Vector

import Data.AffineSpace
import Data.VectorSpace

data Frame2 a = Frame2 (Point2 a) (Vec2 a) (Vec2 a) 
  deriving (Eq,Show)

type DFrame2 = Frame2 Double


data Frame3 a = Frame3 (Point3 a) (Vec3 a) (Vec3 a) (Vec3 a)
  deriving (Eq,Show)


type DFrame3 = Frame3 Double



class Ortho fr where
  type Point fr :: *
  ortho :: Point fr -> fr


instance Num a => Ortho (Frame2 a) where
  type Point (Frame2 a) = Point2 a
  ortho ogin = Frame2 ogin (V2 1 0) (V2 0 1)


--------------------------------------------------------------------------------
-- operations


-- Is the frame orthonormal - i.e. are all basis vectors orthogonal
-- and each is of unit length?

orthonormalFrame :: (Floating a, InnerSpace a, a ~ Scalar a) => Frame2 a -> Bool
orthonormalFrame (Frame2 _ xv yv) = 
  orthogonal xv yv && euclidianNorm xv == 1 && euclidianNorm yv == 1

class OrthF fr where
  orthF :: (Floating a, InnerSpace a, a ~ Scalar a) => fr a -> Bool

instance OrthF Frame2 where
  orthF = orthonormalFrame

instance OrthF Frame3 where
  orthF (Frame3 _ xv yv zv) = 
    orthogonal xv yv && orthogonal yv zv && orthogonal zv xv 
                     && euclidianNorm xv == 1
                     && euclidianNorm yv == 1
                     && euclidianNorm zv == 1



-- Given a point and a frame, return the point in world coordinates
pointInWorld :: (Num a, AffineSpace a, VectorSpace a, 
                 Scalar a ~ a, a ~ Diff a)
             => Point2 a -> Frame2 a -> Point2 a
pointInWorld (P2 x y) (Frame2 o e0 e1) = (o .+^ (x *^ e0)) .+^ (y *^ e1)

vectorInWorld :: (Num a, AffineSpace a, VectorSpace a, 
                 Scalar a ~ a, a ~ Diff a)
             => Vec2 a -> Frame2 a -> Vec2 a
vectorInWorld (V2 x y) (Frame2 _ e0 e1) = (x *^ e0) + (y *^ e1)






-- frame to frame
ftof :: (Num a, AffineSpace a, InnerSpace a, Scalar a ~ a, a ~ Diff a) 
     => Frame2 a -> Frame2 a -> Matrix3'3 a
ftof (Frame2 o xv yv) (Frame2 o' xv' yv') = 
     M3'3 (xv' <.> xv) (yv' <.> xv) ((o' .-. o) <.> xv)
          (xv' <.> yv) (yv' <.> yv) ((o' .-. o) <.> yv)
          0            0            1
