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
-- changed at revision 0.50.0 to the current \"d\"-prefixed names.
-- This allows higher-level frameworks to define their own 
-- functions or class-methods using the obvious good names 
-- (@rotate@, @scale@ etc.). The derived operations (@rotate30@, 
-- @uniformScale, etc.) have been removed as a higher-level 
-- implementation is expected to re-implement them accounting for 
-- polymorphic units as necessary.
--  
--------------------------------------------------------------------------------

module Wumpus.Core.AffineTrans
  ( 
  -- * Type classes
    DTransform(..)
  , DRotate(..)
  , DRotateAbout(..)
  , DScale(..)
  , DTranslate(..)


  ) where

import Wumpus.Core.Geometry



--------------------------------------------------------------------------------
-- Affine transformations 

-- | Apply a matrix transformation directly.
--
class DTransform t where
  dtransform :: DMatrix3'3 -> t -> t


instance DTransform () where
  dtransform _ = id

instance DTransform a => DTransform (Maybe a) where
  dtransform = fmap . dtransform

instance (DTransform a, DTransform b) => 
    DTransform (a,b)  where
  dtransform mtrx (a,b) = (dtransform mtrx a, dtransform mtrx b)


instance DTransform (Point2 Double) where
  dtransform ctm = (ctm *#)

instance DTransform (Vec2 Double) where
  dtransform ctm = (ctm *#)


--------------------------------------------------------------------------------

-- | Type class for rotation.
-- 
class DRotate t where
  drotate :: Radian -> t -> t

instance DRotate () where
  drotate _ = id

instance DRotate a => DRotate (Maybe a) where
  drotate = fmap . drotate


instance (DRotate a, DRotate b) => DRotate (a,b)  where
  drotate ang (a,b) = (drotate ang a, drotate ang b)


instance DRotate (Point2 Double) where
  drotate ang = ((rotationMatrix ang) *#)

instance DRotate (Vec2 Double) where
  drotate ang = ((rotationMatrix ang) *#)

--
--

-- | Type class for rotation about a point.
--
-- Note - the point is a @DPoint2@ - i.e. it has PostScript points
-- for x and y-units.
--
class DRotateAbout t where
  drotateAbout :: Radian -> DPoint2 -> t -> t


instance DRotateAbout () where
  drotateAbout _ _ = id

instance DRotateAbout a => DRotateAbout (Maybe a) where
  drotateAbout ang pt = fmap (drotateAbout ang pt)


instance (DRotateAbout a, DRotateAbout b) => DRotateAbout (a,b) where
  drotateAbout ang pt (a,b) = (drotateAbout ang pt a, drotateAbout ang pt b)

instance DRotateAbout (Point2 Double) where
  drotateAbout ang pt = ((originatedRotationMatrix ang pt) *#) 


instance DRotateAbout (Vec2 Double) where
  drotateAbout ang pt = ((originatedRotationMatrix ang pt) *#) 
  
--------------------------------------------------------------------------------
-- Scale

-- | Type class for scaling.
--
class DScale t where
  dscale :: Double -> Double -> t -> t


instance DScale () where
  dscale _ _ = id

instance DScale a => DScale (Maybe a) where
  dscale sx sy = fmap (dscale sx sy)

instance (DScale a, DScale b) => DScale (a,b) where
  dscale sx sy (a,b) = (dscale sx sy a, dscale sx sy b)

instance DScale (Point2 Double) where
  dscale sx sy = ((scalingMatrix sx sy) *#)

instance DScale (Vec2 Double) where
  dscale sx sy = ((scalingMatrix sx sy) *#)

--------------------------------------------------------------------------------
-- Translate

-- | Type class for translation.
--
class DTranslate t where
  dtranslate :: Double -> Double -> t -> t

instance DTranslate a => DTranslate (Maybe a) where
  dtranslate dx dy = fmap (dtranslate dx dy)

instance (DTranslate a, DTranslate b) => 
    DTranslate (a,b) where
  dtranslate dx dy (a,b) = (dtranslate dx dy a, dtranslate dx dy b)

instance DTranslate (Point2 Double) where
  dtranslate dx dy (P2 x y) = P2 (x + dx) (y + dy)

instance DTranslate (Vec2 Double) where
  dtranslate dx dy (V2 x y) = V2 (x + dx) (y + dy)


