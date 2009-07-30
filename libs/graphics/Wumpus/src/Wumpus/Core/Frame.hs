{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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


module Wumpus.Core.Frame 
  ( 
  -- * Frame types
    Frame2(..)
  , DFrame2
  , Frame3(..)
  , DFrame3

  -- * Frame type classes
  , Frame(..)

  -- * Operations
  , withinFrame

  , inFrame

  ) where

import Wumpus.Core.Instances ()
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Vector

import Data.AffineSpace
import Data.VectorSpace

--------------------------------------------------------------------------------
-- Frame types and standard instances

-- | Two dimensional frame.
data Frame2 a = Frame2 (Point2 a) (Vec2 a) (Vec2 a) 
  deriving (Eq,Show)

type DFrame2 = Frame2 Double


-- | Three dimensional frame.
data Frame3 a = Frame3 (Point3 a) (Vec3 a) (Vec3 a) (Vec3 a)
  deriving (Eq,Show)


type DFrame3 = Frame3 Double


-- Given that the two frames defined above are paramteric /inside/ both points 
-- and vectors is there a sensible use of fmap on them?



--------------------------------------------------------------------------------
-- Frame type classes


-- | Frame operations dependent on the origin\'s Point type
--
-- @ortho@ - create an orthogonal frame at the supplied point.
--
-- @origin@ - extract the origin of a frame
--
-- @displaceOrigin@ - move the origin by the vector.
--
-- @orthonormalFrame@ - Is the frame orthonormal - i.e. are all 
-- basis vectors orthogonal and each is of unit length?
--
-- @coord@ - extract the coordinates of the given point within the 
-- reference frame in the standard Catesian frame
--  

class Frame fr where
  type Point fr :: * -> *
  type Vec fr :: * -> *
  ortho            :: Num a => Point fr a -> fr a
  origin           :: Num a => fr a -> Point fr a
  displaceOrigin   :: Num a => Vec fr a -> fr a -> fr a
  orthonormalFrame :: (Floating a, InnerSpace a, a ~ Scalar a) => fr a -> Bool
  coord :: (Num a, VectorSpace a, a ~ Scalar a) => fr a -> Point fr a -> Point fr a


--------------------------------------------------------------------------------
-- Instances


instance Frame Frame2  where
  type Point Frame2 = Point2
  type Vec   Frame2 = Vec2 

  ortho ogin = Frame2 ogin (V2 1 0) (V2 0 1)

  origin (Frame2 o _ _) = o

  displaceOrigin v (Frame2 o vx vy) = Frame2 (o.+^v) vx vy

  orthonormalFrame (Frame2 _ xv yv) = 
      orthogonal xv yv && euclidianNorm xv == 1 && euclidianNorm yv == 1

  coord (Frame2 o e0 e1) (P2 x y) = (o .+^ (x *^ e0)) .+^ (y *^ e1)


instance Frame Frame3 where
  type Point Frame3 = Point3
  type Vec   Frame3 = Vec3

  ortho ogin = Frame3 ogin (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

  origin (Frame3 o _ _ _) = o

  displaceOrigin v (Frame3 o vx vy vz) = Frame3 (o.+^v) vx vy vz

  orthonormalFrame (Frame3 _ xv yv zv) = 
      orthogonal xv yv && orthogonal yv zv && orthogonal zv xv 
                       && euclidianNorm xv == 1
                       && euclidianNorm yv == 1
                       && euclidianNorm zv == 1

  coord (Frame3 o e0 e1 e2) (P3 x y z) = ((o .+^ (x*^e0)) .+^ (y*^e1)) .+^ (z*^e2) 


--------------------------------------------------------------------------------
-- Operations


-- | coord pointwise ...
withinFrame :: (Num a, Pointwise (s a), VectorSpace a, AffineSpace a,
                Pt (s a) ~ Point2 a, Scalar a ~ a) 
            => Frame2 a -> s a -> s a
withinFrame frm s = pointwise (coord frm) s


-- I've forgotten the point of this one though it is needed by Drawing.Arrow

-- | point/vector in frame
inFrame :: (Floating a, VectorSpace a, MatrixMult Matrix3'3 t, a ~ Scalar a) 
     => Frame2 a -> t a -> t a 
inFrame frm p = (frameMinv frm) *# p



frameMinv :: (Fractional a, VectorSpace a, a ~ Scalar a) 
          => Frame2 a -> Matrix3'3 a
frameMinv (Frame2 (P2 x y) (V2 v01 v02) (V2 v11 v12)) = 
  inverse $ M3'3 v01  v11 x
                 v02  v12 y
                 0    0   1




