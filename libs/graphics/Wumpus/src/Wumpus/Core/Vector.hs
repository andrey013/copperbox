{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Vector
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Vector type
--
--------------------------------------------------------------------------------


module Wumpus.Core.Vector
  (
  -- * Vector types
    Vec2(..)
  , DVec2
  , Vec3(..)
  , DVec3

  -- * Vector type classes
  , EuclidianNorm(..)
  , Independent(..)
  , HVec(..)
  , VVec(..)


  -- * Construct
  , freeVector
  , avec2
  , bisector
  
  -- * Operations
  , interiorAngle
  , vangle
  , direction2
  , perpendicular
  , orthogonal
  , horizontal
  , vertical

  ) where

import Wumpus.Core.Fun
import Wumpus.Core.Geometric
import Wumpus.Core.Pointwise
import Wumpus.Core.Radian

import Data.AffineSpace
import Data.VectorSpace

import Data.Monoid

data Vec2 a = V2 !a !a
  deriving (Eq,Show)

type DVec2 = Vec2 Double

data Vec3 a = V3 !a !a !a
  deriving (Eq,Show)

type DVec3 = Vec3 Double 

instance Functor Vec2 where
  fmap f (V2 a b) = V2 (f a) (f b)

instance Functor Vec3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)

--------------------------------------------------------------------------------
-- Type class instances


-- Vectors have a sensible Monoid instance as addition

instance Num a => Monoid (Vec2 a) where
  mempty = V2 0 0
  V2 a b `mappend` V2 x y = V2 (a+x) (b+y) 

instance Num a => Monoid (Vec3 a) where
  mempty = V3 0 0 0
  V3 a b c `mappend` V3 x y z = V3 (a+x) (b+y) (c+z)

 
instance Pointwise (Vec2 a) where
  type Pt (Vec2 a) = Vec2 a
  pointwise f v = f v

instance Pointwise (Vec3 a) where
  type Pt (Vec3 a) = Vec3 a
  pointwise f v = f v

instance Direction2 Vec2 where
   direction2 (V2 a b) = toRadian $ atan (a/b)

-- Converse of a vector is same magintude, opposite direction
instance Num a => Converse (Vec2 a) where
  converse (V2 a b) = V2 (-a) (-b)


--------------------------------------------------------------------------------
-- Vector space and related instances

instance Num a => AdditiveGroup (Vec2 a) where
  zeroV = V2 0 0 
  (^+^) = mappend
  negateV (V2 a b) = V2 (-a) (-b) 


instance Num a => AdditiveGroup (Vec3 a) where
  zeroV = V3 0 0 0
  (^+^) = mappend
  negateV (V3 a b c) = V3 (-a) (-b) (-c)


instance Num a => VectorSpace (Vec2 a) where
  type Scalar (Vec2 a) = a
  s *^ (V2 a b) = V2 (s*a) (s*b)

instance Num a => VectorSpace (Vec3 a) where
  type Scalar (Vec3 a) = a
  s *^ (V3 a b c) = V3 (s*a) (s*b) (s*c)



-- scalar (dot / inner) product via the class InnerSpace

instance (Num a, InnerSpace a, Scalar a ~ a) 
    => InnerSpace (Vec2 a) where
  (V2 a b) <.> (V2 a' b') = (a <.> a') ^+^ (b <.> b')



instance (Num a, InnerSpace a, Scalar a ~ a) 
    => InnerSpace (Vec3 a) where
  (V3 a b c) <.> (V3 a' b' c') = (a <.> a') ^+^ (b <.> b') ^+^ (c <.> c')


 
--------------------------------------------------------------------------------
-- Vector type classes


class EuclidianNorm t where
  euclidianNorm :: Floating a => t a -> a

instance EuclidianNorm Vec2 where
  euclidianNorm (V2 a b) = sqrt $ (a*a) + (b*b)


-- in 3D the norm is the square root of the dot product 
-- sqrt (v<.>v), or sqrt (a1^2) (a2^2) (a3^2)

instance EuclidianNorm Vec3 where
  euclidianNorm (V3 a b c) = sqrt $ (a*a) + (b*b) + (c*c)



class Independent t where
  independent :: Fractional a => t a -> t a -> Bool
  

instance Independent Vec2 where
  independent (V2 a1 a2) (V2 b1 b2) = (a1/a2) /= (b1/b2)


-- Alternatively these could be built as
--   hvec :: (Num a, Scalar v ~ a) => a -> v

-- | Construct a vector with horizontal displacement
-- and test the sign of the horizontal component
class HVec v where 
  hvec       :: (Scalar v ~ a) => a -> v
  hsignum    :: (Scalar v ~ a) => v -> a 

instance Num a => HVec (Vec2 a) where
  hvec d                = V2 d 0
  hsignum (V2 a _)      = signum a

instance Num a => HVec (Vec3 a) where
  hvec d                = V3 d 0 0
  hsignum (V3 a _ _)    = signum a

-- | Construct a vector with vertical displacement 
-- and test the sign of the vertical component
class VVec v where
  vvec    :: (Scalar v ~ a) => a -> v
  vsignum :: (Scalar v ~ a) => v -> a

instance Num a => VVec (Vec2 a) where
  vvec d                = V2 0 d
  vsignum (V2 _ b)      = signum b

instance Num a => VVec (Vec3 a) where
  vvec d                = V3 0 d 0
  vsignum (V3 _ b _)    = signum b



--------------------------------------------------------------------------------
-- Construction

-- | The free vector from point @a@ to point @b@.
freeVector :: AffineSpace pt => pt -> pt -> Diff pt
freeVector a b = b .-. a 


-- | Construct a vector from and angle and a magnitude
avec2 :: Floating a => Radian -> a -> Vec2 a
avec2 theta d = V2 x y where
  ang = fromRadian theta
  x   = d * cos ang
  y   = d * sin ang


-- | Construct the vector that bisects the vectors @u@ and @v@.
bisector :: (VectorSpace v, Fractional a, Scalar v ~ a) => v -> v -> v
bisector u v = u ^+^ ((v ^-^ u) ^* 0.5)



--------------------------------------------------------------------------------
-- Operations

-- | Interior angle between two vector
interiorAngle :: (Real a, Floating a, InnerSpace v, Scalar v ~ a) 
              => v -> v -> Radian
interiorAngle u v = toRadian $ acos ((u <.> v) / ((magnitude u) * (magnitude v)))    


-- | CCW angle between the vector and the horizontal plane.
vangle :: (HVec v, VVec v, Ord a, Floating a, Real a, InnerSpace v, 
           Scalar v ~ a) 
       => v -> Radian
vangle v | vsignum v >= 0 = interiorAngle v (hvec 1)
         | otherwise      = 2*pi - interiorAngle v (hvec 1)
  
-- Test whether the vectors are perpendicular
-- two vectors in R2 are perpendicular iff their dot product is 0
perpendicular :: (Floating a, InnerSpace v, Scalar v ~ a) 
              => v -> v -> Bool
perpendicular = (==0) `oo` (<.>)

-- alternative name for perpendicular
orthogonal :: (Floating a, InnerSpace v, Scalar v ~ a) 
              => v -> v -> Bool
orthogonal = (==0) `oo` (<.>)


-- | Derive the horizontal component
horizontal :: Num a => Vec2 a -> Vec2 a
horizontal (V2 a _) = V2 a 0


-- | Derive the vertical component
vertical :: Num a => Vec2 a -> Vec2 a
vertical (V2 _ b) = V2 0 b


--------------------------------------------------------------------------------

-- note normal function in Data.Cross






