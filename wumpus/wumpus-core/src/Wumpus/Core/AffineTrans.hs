{-# LANGUAGE FlexibleInstances          #-}
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
--
-- Design note - the formulation of the affine classes is not 
-- ideal as dealing with units is avoided and the instances for
-- Point2 and Vec2 are only applicable to @DPoint2@ and @DVec2@.
-- Dealing with units is avoided as some useful units 
-- (particulary Em and En) have contextual interterpretations - 
-- i.e. their size is dependent on the current font size - and so 
-- they cannot be accommodated without some monadic context.
-- 
-- For this reason, the naming scheme for the affine classes was
-- changed at revision 0.50.0 to the current long-winded names.
-- This allows higher-level frameworks to define their own 
-- functions or class-methods using the obvious good names 
-- (@rotate@, @scale@ etc.). The derived operations (@rotate30@, 
-- @uniformScale, etc.) have been removed, an higher-level 
-- implementation is expected to re-implement them accounting for 
-- units as necessary.
--  
--------------------------------------------------------------------------------

module Wumpus.Core.AffineTrans
  ( 
  -- * Type classes
    AffineTransform(..)
  , AffineRotate(..)
  , AffineRotateAbout(..)
  , AffineScale(..)
  , AffineTranslate(..)


  ) where

import Wumpus.Core.Geometry



--------------------------------------------------------------------------------
-- Affine transformations 

-- | Apply a matrix transformation directly.
--
class AffineTransform t where
  affineTransform :: DMatrix3'3 -> t -> t


instance AffineTransform () where
  affineTransform _ = id

instance AffineTransform a => AffineTransform (Maybe a) where
  affineTransform = fmap . affineTransform

instance (AffineTransform a, AffineTransform b) => 
    AffineTransform (a,b)  where
  affineTransform mtrx (a,b) = (affineTransform mtrx a, affineTransform mtrx b)


instance AffineTransform (Point2 Double) where
  affineTransform ctm = (ctm *#)

instance AffineTransform (Vec2 Double) where
  affineTransform ctm = (ctm *#)


--------------------------------------------------------------------------------

-- | Type class for rotation.
-- 
class AffineRotate t where
  affineRotate :: Radian -> t -> t

instance AffineRotate () where
  affineRotate _ = id

instance AffineRotate a => AffineRotate (Maybe a) where
  affineRotate = fmap . affineRotate


instance (AffineRotate a, AffineRotate b) => AffineRotate (a,b)  where
  affineRotate ang (a,b) = (affineRotate ang a, affineRotate ang b)


instance AffineRotate (Point2 Double) where
  affineRotate ang = ((rotationMatrix ang) *#)

instance AffineRotate (Vec2 Double) where
  affineRotate ang = ((rotationMatrix ang) *#)

--
--

-- | Type class for rotation about a point.
--
-- Note - the point is a @DPoint2@ - i.e. it has PostScript points
-- for x and y-units.
--
class AffineRotateAbout t where
  affineRotateAbout :: Radian -> DPoint2 -> t -> t


instance AffineRotateAbout () where
  affineRotateAbout _ _ = id

instance AffineRotateAbout a => AffineRotateAbout (Maybe a) where
  affineRotateAbout ang pt = fmap (affineRotateAbout ang pt)


instance (AffineRotateAbout a, AffineRotateAbout b) => 
    AffineRotateAbout (a,b) where
  affineRotateAbout ang pt (a,b) = ( affineRotateAbout ang pt a
                                   , affineRotateAbout ang pt b )

instance AffineRotateAbout (Point2 Double) where
  affineRotateAbout ang pt = ((originatedRotationMatrix ang pt) *#) 


instance AffineRotateAbout (Vec2 Double) where
  affineRotateAbout ang pt = ((originatedRotationMatrix ang pt) *#) 
  
--------------------------------------------------------------------------------
-- Scale

-- | Type class for scaling.
--
class AffineScale t where
  affineScale :: Double -> Double -> t -> t


instance AffineScale () where
  affineScale _ _ = id

instance AffineScale a => AffineScale (Maybe a) where
  affineScale sx sy = fmap (affineScale sx sy)

instance (AffineScale a, AffineScale b) => AffineScale (a,b) where
  affineScale sx sy (a,b) = (affineScale sx sy a, affineScale sx sy b)

instance AffineScale (Point2 Double) where
  affineScale sx sy = ((scalingMatrix sx sy) *#)

instance AffineScale (Vec2 Double) where
  affineScale sx sy = ((scalingMatrix sx sy) *#)

--------------------------------------------------------------------------------
-- Translate

-- | Type class for translation.
--
class AffineTranslate t where
  affineTranslate :: Double -> Double -> t -> t

instance AffineTranslate a => AffineTranslate (Maybe a) where
  affineTranslate dx dy = fmap (affineTranslate dx dy)

instance (AffineTranslate a, AffineTranslate b) => 
    AffineTranslate (a,b) where
  affineTranslate dx dy (a,b) = (affineTranslate dx dy a, affineTranslate dx dy b)

instance AffineTranslate (Point2 Double) where
  affineTranslate dx dy (P2 x y) = P2 (x + dx) (y + dy)

instance AffineTranslate (Vec2 Double) where
  affineTranslate dx dy (V2 x y) = V2 (x + dx) (y + dy)


