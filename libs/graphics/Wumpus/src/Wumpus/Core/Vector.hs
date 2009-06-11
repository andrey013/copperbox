{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
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


module Wumpus.Core.Vector where

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


-- Vectors have a sensible Monoid instance as addition

instance Num a => Monoid (Vec2 a) where
  mempty = V2 0 0
  V2 a b `mappend` V2 x y = V2 (a+x) (b+y) 

instance Num a => Monoid (Vec3 a) where
  mempty = V3 0 0 0
  V3 a b c `mappend` V3 x y z = V3 (a+x) (b+y) (c+z)



instance Num a => Num (Vec2 a) where
  (+) (V2 a b) (V2 x y) = V2 (a+x) (b+y)
  (-) (V2 a b) (V2 x y) = V2 (a-x) (b-y)
  (*) (V2 a b) (V2 x y) = V2 (a*x) (b*y)
  abs (V2 a b)          = V2 (abs a) (abs b)
  negate (V2 a b)       = V2 (negate a) (negate b)
  signum (V2 a b)       = V2 (signum a) (signum b)
  fromInteger i         = V2 (fromInteger i) (fromInteger i)


instance Num a => Num (Vec3 a) where
  (+) (V3 a b c) (V3 x y z) = V3 (a+x) (b+y) (c+z)
  (-) (V3 a b c) (V3 x y z) = V3 (a-x) (b-y) (c-z)
  (*) (V3 a b c) (V3 x y z) = V3 (a*x) (b*y) (c*z)
  abs (V3 a b c)            = V3 (abs a) (abs b) (abs c)
  negate (V3 a b c)         = V3 (negate a) (negate b) (negate c)
  signum (V3 a b c)         = V3 (signum a) (signum b) (signum c)
  fromInteger i             = V3 (fromInteger i) (fromInteger i) (fromInteger i)


instance Fractional a => Fractional (Vec2 a) where
  (/) (V2 a b) (V2 x y) = V2 (a/x) (b/y)
  recip (V2 a b)        = V2 (recip a) (recip b)
  fromRational a        = V2 (fromRational a) (fromRational a)

instance Fractional a => Fractional (Vec3 a) where
  (/) (V3 a b c) (V3 x y z) = V3 (a/x) (b/y) (c/z)
  recip (V3 a b c)          = V3 (recip a) (recip b) (recip c)
  fromRational a            = V3 (fromRational a) (fromRational a) (fromRational a)




class EuclidianNorm t where
  euclidianNorm :: Floating a => t a -> a

instance EuclidianNorm Vec2 where
  euclidianNorm (V2 a b) = sqrt $ (a*a) + (b*b)

-- direction in 2D space

class Direction2 t where   
   direction :: Floating a => t a -> a

instance Direction2 Vec2 where
   direction (V2 a b) = atan (a/b)


-- This needs a more general signature...
-- magnitude :: (Floating a, InnerSpace (t a), t ~ Scalar a) => t a -> a
magnitude :: Vec2 Double -> Double
magnitude v@(V2 _ _) = sqrt (v <.> v)

instance Num a => AdditiveGroup (Vec2 a) where
  zeroV = V2 0 0 
  (^+^) = (+)
  negateV = negate


instance Num a => AdditiveGroup (Vec3 a) where
  zeroV = V3 0 0 0
  (^+^) = (+)
  negateV = negate


instance (Num a, VectorSpace a) => VectorSpace (Vec2 a) where
  type Scalar (Vec2 a) = Scalar a
  s *^ (V2 a b) = V2 (s*^a) (s*^b)

instance (Num a, VectorSpace a) => VectorSpace (Vec3 a) where
  type Scalar (Vec3 a) = Scalar a
  s *^ (V3 a b c) = V3 (s*^a) (s*^b) (s*^c)



-- scalar (dot / inner) product via the class InnerSpace

instance (Num a, InnerSpace a, AdditiveGroup (Scalar a)) 
    => InnerSpace (Vec2 a) where
  (V2 a b) <.> (V2 a' b') = (a <.> a') ^+^ (b <.> b')



instance (Num a, InnerSpace a, AdditiveGroup (Scalar a)) 
    => InnerSpace (Vec3 a) where
  (V3 a b c) <.> (V3 a' b' c') = (a <.> a') ^+^ (b <.> b') ^+^ (c <.> c')





-- two vectors in R2 are perpendicular iff their dot product is 0
perp :: DVec2 -> DVec2 -> Bool
perp = ((==0) .) . (<.>)








