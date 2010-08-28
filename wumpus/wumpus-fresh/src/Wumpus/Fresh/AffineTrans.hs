{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Affine.AffineTrans
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Fresh affine transformations.
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Fresh.AffineTrans
  ( 
  -- * Type classes
    Transform(..)
  , Rotate(..)
  , RotateAbout(..)
  , Scale(..)
  , Translate(..)

  -- * Common rotations
  , rotate30
  , rotate30About
  , rotate45
  , rotate45About
  , rotate60
  , rotate60About
  , rotate90
  , rotate90About
  , rotate120
  , rotate120About
  
  -- * Common scalings
  , uniformScale
  , reflectX
  , reflectY

  -- * Translate by a vector
  , translateBy
  
  -- * Reflections in supplied plane rather than about the origin
  , reflectXPlane
  , reflectYPlane   

  ) where

import Wumpus.Fresh.Geometry



--------------------------------------------------------------------------------
-- Affine transformations 

-- | Apply a matrix trasnformation directly.
class Transform t where
  transform :: u ~ DUnit t => Matrix3'3 u -> t -> t



-- | Type class for rotation.
-- 
class Rotate t where
  rotate :: Radian -> t -> t

instance Num u => Transform (Point2 u) where
  transform ctm = (ctm *#)

instance Num u => Transform (Vec2 u) where
  transform ctm = (ctm *#)


instance (Floating u, Real u) => Rotate (Point2 u) where
  rotate a = ((rotationMatrix a) *#)

instance (Floating u, Real u) => Rotate (Vec2 u) where
  rotate a = ((rotationMatrix a) *#)


-- | Type class for rotation about a point.
class RotateAbout t where
  rotateAbout :: u ~ DUnit t =>  Radian -> Point2 u -> t -> t 


instance (Floating u, Real u) => RotateAbout (Point2 u) where
  rotateAbout a pt = ((originatedRotationMatrix a pt) *#) 


instance (Floating u, Real u) => RotateAbout (Vec2 u) where
  rotateAbout a pt = ((originatedRotationMatrix a pt) *#) 
  
--------------------------------------------------------------------------------
-- Scale

-- | Type class for scaling.
class Scale t where
  scale :: u ~ DUnit t => u -> u -> t -> t

instance Num u => Scale (Point2 u) where
  scale x y = ((scalingMatrix x y) *#) 

instance Num u => Scale (Vec2 u) where
  scale x y = ((scalingMatrix x y) *#) 

--------------------------------------------------------------------------------
-- Translate

-- | Type class for translations.
class Translate t where
  translate :: DUnit t -> DUnit t -> t -> t

instance Num u => Translate (Point2 u) where
  translate x y = ((translationMatrix x y) *#)

instance Num u => Translate (Vec2 u) where
  translate x y = ((translationMatrix x y) *#)


-------------------------------------------------------------------------------- 
-- Common rotations



-- | Rotate by 30 degrees about the origin. 
rotate30 :: Rotate t => t -> t 
rotate30 = rotate (pi/6) 

-- | Rotate by 30 degrees about the supplied point.
rotate30About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate30About = rotateAbout (pi/6)

-- | Rotate by 45 degrees about the origin. 
rotate45 :: Rotate t => t -> t 
rotate45 = rotate (pi/4) 

-- | Rotate by 45 degrees about the supplied point.
rotate45About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate45About = rotateAbout (pi/4)

-- | Rotate by 60 degrees about the origin. 
rotate60 :: Rotate t => t -> t 
rotate60 = rotate (2*pi/3) 

-- | Rotate by 60 degrees about the supplied point.
rotate60About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate60About = rotateAbout (2*pi/3)

-- | Rotate by 90 degrees about the origin. 
rotate90 :: Rotate t => t -> t 
rotate90 = rotate (pi/2) 

-- | Rotate by 90 degrees about the supplied point.
rotate90About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate90About = rotateAbout (pi/2)

-- | Rotate by 120 degrees about the origin. 
rotate120 :: Rotate t => t -> t 
rotate120 = rotate (4*pi/3) 

-- | Rotate by 120 degrees about the supplied point.
rotate120About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate120About = rotateAbout (4*pi/3)



--------------------------------------------------------------------------------
-- Common scalings

-- | Scale both x and y dimensions by the same amount.
uniformScale :: (Scale t, DUnit t ~ u) => u -> t -> t 
uniformScale a = scale a a 

-- | Reflect in the X-plane about the origin.
reflectX :: (Num u, Scale t, DUnit t ~ u) => t -> t
reflectX = scale (-1) 1

-- | Reflect in the Y-plane about the origin.
reflectY :: (Num u, Scale t, DUnit t ~ u) => t -> t
reflectY = scale 1 (-1)

--------------------------------------------------------------------------------
-- translations

-- | Translate by the x and y components of a vector.
translateBy :: (Translate t, DUnit t ~ u) => Vec2 u -> t -> t 
translateBy (V2 x y) = translate x y


--------------------------------------------------------------------------------
-- Translation and scaling

-- | Reflect in the X plane that intersects the supplied point. 
reflectXPlane :: (Num u, Scale t, Translate t, u ~ DUnit t) 
              => Point2 u -> t -> t
reflectXPlane (P2 x y) = translate x y . scale (-1) 1 . translate (-x) (-y)

-- | Reflect in the Y plane that intersects the supplied point.
reflectYPlane :: (Num u, Scale t, Translate t, u ~ DUnit t) 
              => Point2 u -> t -> t
reflectYPlane (P2 x y) = translate x y . scale 1 (-1) . translate (-x) (-y)
