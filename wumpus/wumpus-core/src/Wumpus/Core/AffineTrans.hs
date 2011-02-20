{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE UndecidableInstances       #-}


------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.AffineTrans
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Affine transformations.
-- 
-- The common affine transformations represented as type classes -
-- scaling, rotation, translation.
--
--
-- Internally, when a Picture is composed and transformed, Wumpus
-- only transforms the bounding box - transformations of the 
-- picture content (paths or text labels) are communicated to 
-- PostScript or SVG for final rendering. This is because Wumpus 
-- has no access to the paths that make fonts so cannot transform 
-- them directly.
--
-- Other elements - Vectors, Points, BoundingBoxes and Primtives - 
-- are also instances of the affine classes. However, generally 
-- Wumpus transforms these elements directly rather than 
-- delegating the transformation to PostScript or SVG (the 
-- situation for the Label primitive is more complicated - the 
-- /start/ point is transformed by Wumpus but a matrix 
-- transformation is sent to PostScript to manipulate the opaque 
-- character objects).
--
-- Note - transformations on Primitives are applied to the control 
-- points of the primitive not the /drawing/. A scaled, stroked 
-- path will be drawn with at the standard line width rather than 
-- with a thicker line. Also, text may not render pleasantly after 
-- it has been transformed, PostScript references seem to caution 
-- against transforming text and recommend changing @/scalefont@ 
-- instead of scaling via a transfomation. 
-- 
-- To generate efficient PostScript, Wumpus relies on the matrix
-- representations of the affine transformations being invertible.
-- Do not scale elements by zero!
--
--------------------------------------------------------------------------------

module Wumpus.Core.AffineTrans
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

import Wumpus.Core.Geometry



--------------------------------------------------------------------------------
-- Affine transformations 

-- | Apply a matrix transformation directly.
--
class Transform t where
  transform :: u ~ DUnit t => Matrix3'3 u -> t -> t

instance Transform (UNil u) where
  transform _ = id

instance Num u => Transform (Point2 u) where
  transform ctm = (ctm *#)

instance Num u => Transform (Vec2 u) where
  transform ctm = (ctm *#)

--------------------------------------------------------------------------------

-- | Type class for rotation.
-- 
class Rotate t where
  rotate :: Radian -> t -> t

instance Rotate (UNil u) where
  rotate _ = id

instance Rotate a => Rotate (Maybe a) where
  rotate = fmap . rotate

instance (Rotate a, Rotate b, u ~ DUnit a, u ~ DUnit b) => Rotate (a,b) where
  rotate ang (a,b) = (rotate ang a, rotate ang b)


instance (Floating u, Real u) => Rotate (Point2 u) where
  rotate ang = ((rotationMatrix ang) *#)

instance (Floating u, Real u) => Rotate (Vec2 u) where
  rotate ang = ((rotationMatrix ang) *#)


-- | Type class for rotation about a point.
--
class RotateAbout t where
  rotateAbout :: u ~ DUnit t =>  Radian -> Point2 u -> t -> t 


instance RotateAbout (UNil u) where
  rotateAbout _ _ = id

instance RotateAbout a => RotateAbout (Maybe a) where
  rotateAbout ang pt = fmap (rotateAbout ang pt)

instance (RotateAbout a, RotateAbout b, u ~ DUnit a, u ~ DUnit b) => 
    RotateAbout (a,b) where
  rotateAbout ang pt (a,b) = (rotateAbout ang pt a, rotateAbout ang pt b)



instance (Floating u, Real u) => RotateAbout (Point2 u) where
  rotateAbout ang pt = ((originatedRotationMatrix ang pt) *#) 


instance (Floating u, Real u) => RotateAbout (Vec2 u) where
  rotateAbout ang pt = ((originatedRotationMatrix ang pt) *#) 
  
--------------------------------------------------------------------------------
-- Scale

-- | Type class for scaling.
--
class Scale t where
  scale :: u ~ DUnit t => u -> u -> t -> t

instance Scale (UNil u) where
  scale _ _ = id

instance Scale a => Scale (Maybe a) where
  scale sx sy = fmap (scale sx sy)

instance (Scale a, Scale b, u ~ DUnit a, u ~ DUnit b) => Scale (a,b) where
  scale sx sy (a,b) = (scale sx sy a, scale sx sy b)

instance Num u => Scale (Point2 u) where
  scale sx sy = ((scalingMatrix sx sy) *#) 

instance Num u => Scale (Vec2 u) where
  scale sx sy = ((scalingMatrix sx sy) *#) 

--------------------------------------------------------------------------------
-- Translate

-- | Type class for translation.
--
class Translate t where
  translate :: u ~ DUnit t => u -> u -> t -> t


instance Translate (UNil u) where
  translate _ _ = id

instance (Translate a, Translate b, u ~ DUnit a, u ~ DUnit b) => 
    Translate (a,b) where
  translate dx dy (a,b) = (translate dx dy a, translate dx dy b)


instance Translate a => Translate (Maybe a) where
  translate dx dy = fmap (translate dx dy)

instance Num u => Translate (Point2 u) where
  translate dx dy (P2 x y) = P2 (x+dx) (y+dy)

instance Num u => Translate (Vec2 u) where
  translate dx dy (V2 x y) = V2 (x+dx) (y+dy)


-------------------------------------------------------------------------------- 
-- Common rotations



-- | Rotate by 30 degrees about the origin. 
--
rotate30 :: Rotate t => t -> t 
rotate30 = rotate (pi/6) 

-- | Rotate by 30 degrees about the supplied point.
--
rotate30About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate30About = rotateAbout (pi/6)

-- | Rotate by 45 degrees about the origin. 
--
rotate45 :: Rotate t => t -> t 
rotate45 = rotate (pi/4) 

-- | Rotate by 45 degrees about the supplied point.
--
rotate45About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate45About = rotateAbout (pi/4)

-- | Rotate by 60 degrees about the origin. 
--
rotate60 :: Rotate t => t -> t 
rotate60 = rotate (2*pi/3) 

-- | Rotate by 60 degrees about the supplied point.
--
rotate60About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate60About = rotateAbout (2*pi/3)

-- | Rotate by 90 degrees about the origin. 
--
rotate90 :: Rotate t => t -> t 
rotate90 = rotate (pi/2) 

-- | Rotate by 90 degrees about the supplied point.
--
rotate90About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate90About = rotateAbout (pi/2)

-- | Rotate by 120 degrees about the origin. 
--
rotate120 :: Rotate t => t -> t 
rotate120 = rotate (4*pi/3) 

-- | Rotate by 120 degrees about the supplied point.
--
rotate120About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate120About = rotateAbout (4*pi/3)



--------------------------------------------------------------------------------
-- Common scalings

-- | Scale both x and y dimensions by the same amount.
--
uniformScale :: (Scale t, DUnit t ~ u) => u -> t -> t 
uniformScale a = scale a a 

-- | Reflect in the X-plane about the origin.
--
reflectX :: (Num u, Scale t, DUnit t ~ u) => t -> t
reflectX = scale (-1) 1

-- | Reflect in the Y-plane about the origin.
--
reflectY :: (Num u, Scale t, DUnit t ~ u) => t -> t
reflectY = scale 1 (-1)

--------------------------------------------------------------------------------
-- Translations

-- | Translate by the x and y components of a vector.
--
translateBy :: (Translate t, DUnit t ~ u) => Vec2 u -> t -> t 
translateBy (V2 x y) = translate x y


--------------------------------------------------------------------------------
-- Translation and scaling

-- | Reflect in the X plane that intersects the supplied point. 
--
reflectXPlane :: (Num u, Scale t, Translate t, u ~ DUnit t) 
              => Point2 u -> t -> t
reflectXPlane (P2 x y) = translate x y . scale (-1) 1 . translate (-x) (-y)

-- | Reflect in the Y plane that intersects the supplied point.
--
reflectYPlane :: (Num u, Scale t, Translate t, u ~ DUnit t) 
              => Point2 u -> t -> t
reflectYPlane (P2 x y) = translate x y . scale 1 (-1) . translate (-x) (-y)
