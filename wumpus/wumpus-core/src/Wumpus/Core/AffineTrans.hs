{-# OPTIONS -Wall #-}


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
import Wumpus.Core.Units

import Control.Applicative

--
-- Design note - the formulation of the affine classes is not 
-- really ideal. I would prefer them to say something about the 
-- unit type, but forcing them into Functor form makes them 
-- antagonistic to the types in Wumpus-Basic. 
-- 
-- Up to version 0.50.0, the DUnit type family enforced a relation
-- between the object and the units of transformation, however the
-- relation it enforced was incorrect: in the case of scaling the 
-- scaling magnitude was enforced to the same unit type (point, 
-- centimeter) as the object, but this was wrong - scaling should 
-- be a scaling factor (Double is always adequate).
-- 


-- helpers 

inout :: (Functor f, PsDouble u) => (f Double -> f Double) -> f u -> f u
inout f = fmap fromPsDouble . f . fmap toPsDouble


--------------------------------------------------------------------------------
-- Affine transformations 

-- | Apply a matrix transformation directly.
--
class Transform t where
  transform :: DMatrix3'3 -> t -> t


instance Transform () where
  transform _ = id

instance Transform a => Transform (Maybe a) where
  transform = fmap . transform


instance Transform a => Transform (Const a b) where
  transform mtrx (Const a) = Const $ transform mtrx a


instance (Transform a, Transform b) => Transform (a,b)  where
  transform mtrx (a,b) = (transform mtrx a, transform mtrx b)


instance PsDouble u => Transform (Point2 u) where
  transform ctm = inout (ctm *#)

instance PsDouble u => Transform (Vec2 u) where
  transform ctm = inout (ctm *#)


--------------------------------------------------------------------------------

-- | Type class for rotation.
-- 
class Rotate t where
  rotate :: Radian -> t -> t

instance Rotate () where
  rotate _ = id

instance Rotate u => Rotate (Maybe u) where
  rotate = fmap . rotate

instance Rotate a => Rotate (Const a b) where
  rotate ang (Const a) = Const $ rotate ang a

instance (Rotate a, Rotate b) => Rotate (a,b)  where
  rotate ang (a,b) = (rotate ang a, rotate ang b)


instance (Floating u, Real u, PsDouble u) => Rotate (Point2 u) where
  rotate ang = ((rotationMatrix ang) *#)

instance (Floating u, Real u, PsDouble u) => Rotate (Vec2 u) where
  rotate ang = ((rotationMatrix ang) *#)

--
--

-- | Type class for rotation about a point.
--
-- Note this class has a type relation between the unit type of 
-- the object and the point-of-rotation.
--
class RotateAbout t where
  rotateAbout :: PsDouble u  => Radian -> Point2 u -> t -> t


instance RotateAbout () where
  rotateAbout _ _ = id

instance RotateAbout a => RotateAbout (Maybe a) where
  rotateAbout ang pt = fmap (rotateAbout ang pt)


instance RotateAbout a => RotateAbout (Const a b) where
  rotateAbout ang pt (Const a) = Const $ rotateAbout ang pt a

instance (RotateAbout a, RotateAbout b) => 
    RotateAbout (a,b) where
  rotateAbout ang pt (a,b) = (rotateAbout ang pt a, rotateAbout ang pt b)

instance PsDouble u => RotateAbout (Point2 u) where
  rotateAbout ang pt = let pt' = fmap toPsDouble pt 
                       in inout ((originatedRotationMatrix ang pt') *#) 


instance PsDouble u => RotateAbout (Vec2 u) where
  rotateAbout ang pt = let pt' = fmap toPsDouble pt 
                       in inout ((originatedRotationMatrix ang pt') *#) 
  
--------------------------------------------------------------------------------
-- Scale

-- | Type class for scaling.
--
class Scale t where
  scale :: Double -> Double -> t -> t


instance Scale () where
  scale _ _ = id

instance Scale a => Scale (Maybe a) where
  scale sx sy = fmap (scale sx sy)

instance Scale a => Scale (Const a b) where
  scale sx sy (Const a) = Const $ scale sx sy a

instance (Scale a, Scale b) => Scale (a,b) where
  scale sx sy (a,b) = (scale sx sy a, scale sx sy b)

instance PsDouble u => Scale (Point2 u) where
  scale sx sy = inout ((scalingMatrix sx sy) *#)

instance PsDouble u => Scale (Vec2 u) where
  scale sx sy = inout ((scalingMatrix sx sy) *#)

--------------------------------------------------------------------------------
-- Translate

-- | Type class for translation.
--
class Translate t where
  translate :: Double -> Double -> t -> t

instance Translate a => Translate (Maybe a) where
  translate dx dy = fmap (translate dx dy)

instance Translate a => Translate (Const a b) where
  translate dx dy (Const a) = Const $ translate dx dy a

instance (Translate a, Translate b) => 
    Translate (a,b) where
  translate dx dy (a,b) = (translate dx dy a, translate dx dy b)

instance PsDouble u => Translate (Point2 u) where
  translate dx dy (P2 x y) = P2 (x + fromPsDouble dx) (y + fromPsDouble dy)

instance PsDouble u => Translate (Vec2 u) where
  translate dx dy (V2 x y) = V2 (x + fromPsDouble dx) (y + fromPsDouble dy)


-------------------------------------------------------------------------------- 
-- Common rotations



-- | Rotate by 30 degrees about the origin. 
--
rotate30 :: Rotate t => t -> t
rotate30 = rotate (pi/6) 

-- | Rotate by 30 degrees about the supplied point.
--
rotate30About :: (RotateAbout t, PsDouble u) => Point2 u -> t -> t
rotate30About = rotateAbout (pi/6)

-- | Rotate by 45 degrees about the origin. 
--
rotate45 :: Rotate t => t -> t
rotate45 = rotate (pi/4) 

-- | Rotate by 45 degrees about the supplied point.
--
rotate45About :: (RotateAbout t, PsDouble u) => Point2 u -> t -> t
rotate45About = rotateAbout (pi/4)

-- | Rotate by 60 degrees about the origin. 
--
rotate60 :: Rotate t => t -> t
rotate60 = rotate (2*pi/3) 

-- | Rotate by 60 degrees about the supplied point.
--
rotate60About :: (RotateAbout t, PsDouble u) => Point2 u -> t -> t
rotate60About = rotateAbout (2*pi/3)

-- | Rotate by 90 degrees about the origin. 
--
rotate90 :: Rotate t => t -> t
rotate90 = rotate (pi/2) 

-- | Rotate by 90 degrees about the supplied point.
--
rotate90About :: (RotateAbout t, PsDouble u) => Point2 u -> t -> t
rotate90About = rotateAbout (pi/2)

-- | Rotate by 120 degrees about the origin. 
--
rotate120 :: Rotate t => t -> t
rotate120 = rotate (4*pi/3) 

-- | Rotate by 120 degrees about the supplied point.
--
rotate120About :: (RotateAbout t, PsDouble u) => Point2 u -> t -> t
rotate120About = rotateAbout (4*pi/3)



--------------------------------------------------------------------------------
-- Common scalings

-- | Scale both x and y dimensions by the same amount.
--
uniformScale :: Scale t => Double -> t -> t
uniformScale a = scale a a 

-- | Reflect in the X-plane about the origin.
--
reflectX :: Scale t => t -> t
reflectX = scale (-1) 1

-- | Reflect in the Y-plane about the origin.
--
reflectY :: Scale t => t -> t
reflectY = scale 1 (-1)

--------------------------------------------------------------------------------
-- Translations

-- | Translate by the x and y components of a vector.
--
translateBy :: (Translate t, PsDouble u) => Vec2 u -> t -> t
translateBy (V2 x y) = translate (toPsDouble x) (toPsDouble y)


--------------------------------------------------------------------------------
-- Translation and scaling

-- | Reflect in the X plane that intersects the supplied point. 
--
reflectXPlane :: (PsDouble u, Scale t, Translate t) 
              => Point2 u -> t -> t
reflectXPlane pt = go (fmap toPsDouble pt)
  where
    go (P2 x y) = translate x y . scale (-1) 1 . translate (-x) (-y)

-- | Reflect in the Y plane that intersects the supplied point.
--
reflectYPlane :: (PsDouble u, Scale t, Translate t) 
              => Point2 u -> t -> t
reflectYPlane pt = go (fmap toPsDouble pt) 
  where
    go (P2 x y) = translate x y . scale 1 (-1) . translate (-x) (-y)
