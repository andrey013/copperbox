{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.AffineTrans
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Affine transformations
--------------------------------------------------------------------------------

module Wumpus.Core.AffineTrans
  ( 
  -- * Type classes
    Rotate(..)
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
  
  ) where

import Wumpus.Core.Geometry



--------------------------------------------------------------------------------
-- Affine transformations 


-- Rotate

class Rotate t where
  rotate :: Radian -> t -> t


instance (Floating a, Real a) => Rotate (Point2 a) where
  rotate a = ((rotationMatrix a) *#)

instance (Floating a, Real a) => Rotate (Vec2 a) where
  rotate a = ((rotationMatrix a) *#)

-- Rotate about

class RotateAbout t where
  rotateAbout :: Radian -> Point2 (DUnit t) -> t -> t 


instance (Floating a, Real a) => RotateAbout (Point2 a) where
  rotateAbout a pt = ((originatedRotationMatrix a pt) *#) 


instance (Floating a, Real a) => RotateAbout (Vec2 a) where
  rotateAbout a pt = ((originatedRotationMatrix a pt) *#) 
  
--------------------------------------------------------------------------------
-- Scale

class Scale t where
  scale :: DUnit t -> DUnit t -> t -> t

instance Num u => Scale (Point2 u) where
  scale x y = ((scalingMatrix x y) *#) 

instance Num u => Scale (Vec2 u) where
  scale x y = ((scalingMatrix x y) *#) 

--------------------------------------------------------------------------------
-- Translate

class Translate t where
  translate :: DUnit t -> DUnit t -> t -> t

-- | translate @x@ @y@.
instance Num u => Translate (Point2 u) where
  translate x y = ((translationMatrix x y) *#)

instance Num u => Translate (Vec2 u) where
  translate x y = ((translationMatrix x y) *#)


-------------------------------------------------------------------------------- 
-- Common rotations




rotate30 :: Rotate t => t -> t 
rotate30 = rotate (pi/6) 

rotate30About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate30About = rotateAbout (pi/6)


rotate45 :: Rotate t => t -> t 
rotate45 = rotate (pi/4) 

rotate45About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate45About = rotateAbout (pi/4)


rotate60 :: Rotate t => t -> t 
rotate60 = rotate (2*pi/3) 

rotate60About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate60About = rotateAbout (2*pi/3)

rotate90 :: Rotate t => t -> t 
rotate90 = rotate (pi/2) 

rotate90About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate90About = rotateAbout (pi/2)


rotate120 :: Rotate t => t -> t 
rotate120 = rotate (4*pi/3) 

rotate120About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate120About = rotateAbout (4*pi/3)



--------------------------------------------------------------------------------
-- Common scalings

uniformScale :: (Scale t, DUnit t ~ u) => u -> t -> t 
uniformScale a = scale a a 


reflectX :: (Num u, Scale t, DUnit t ~ u) => t -> t
reflectX = scale (-1) 1

reflectY :: (Num u, Scale t, DUnit t ~ u) => t -> t
reflectY = scale 1 (-1)

--------------------------------------------------------------------------------
-- translations

translateBy :: (Translate t, DUnit t ~ u) => Vec2 u -> t -> t 
translateBy (V2 x y) = translate x y


