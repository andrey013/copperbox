{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.BaseObjects
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Aliases for ContextFun types.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.BaseObjects
  (

  -- * Drawing info
    DrawingInfo
  , LocDrawingInfo
  , LocThetaDrawingInfo


  
  -- * Drawing objects
  , ImageAns
  , GraphicAns

  , Image
  , LocImage
  , LocThetaImage

  , DImage
  , DLocImage
  , DLocThetaImage

  , hyperlink

  ) where

import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.WrappedPrimitive

import Wumpus.Core                              -- package: wumpus-core

--------------------------------------------------------------------------------
-- DrawingInfo

-- | A query on the DrawingContext.
--
-- Alias for 'CF'.
-- 
type DrawingInfo a      = CF a


-- | A query on the DrawingContext respective to the supplied
--  point.
--
-- Alias for 'LocCF'.
-- 
type LocDrawingInfo u a   = LocCF u a


-- | A query on the DrawingContext respective to the supplied
--  point and angle.
--
-- Alias for 'LocCF'.
-- 
type LocThetaDrawingInfo u a   = LocThetaCF u a




--------------------------------------------------------------------------------
-- Image


-- | An Image always returns a pair of some polymorphic answer @a@
-- and a PrimGraphic.
--
-- Note a PrimGraphic cannot be empty.
-- 
type ImageAns u a       = (a, PrimGraphic u)


type GraphicAns u       = ImageAns u (UNil u)


-- | Draw a PrimGraphic repsective to the 'DrawingContext' and 
-- return some answer @a@.
-- 
type Image u a      = CF (ImageAns u a)


-- | Draw a PrimGraphic respective to the 'DrawingContext' and 
-- the supplied point, return some answer @a@.
-- 
type LocImage u a   = LocCF u (ImageAns u a)


-- | Draw a PrimGraphic respective to the 'DrawingContext' and
-- the supplied point and angle.
-- 
type LocThetaImage u a   = LocThetaCF u (ImageAns u a)



type DImage a            = Image Double a
type DLocImage a         = LocImage Double a
type DLocThetaImage a    = LocThetaImage Double a


type instance DUnit (Image u a) = u -- GuardEq (DUnit a) (DUnit (PrimGraphic u))

type instance DUnit (LocImage u a) = u --  GuardEq (DUnit a) (DUnit (PrimGraphic u))

type instance DUnit (LocThetaImage u a) = u

--------------------------------------------------------------------------------
-- Affine instances

instance (Real u, Floating u, Rotate a, DUnit a ~ u) => 
    Rotate (Image u a) where
  rotate ang = fmap (rotate ang)


instance (Real u, Floating u, RotateAbout a, DUnit a ~ u) => 
    RotateAbout (Image u a) where
  rotateAbout ang pt = fmap (rotateAbout ang pt)


instance (Num u, Scale a, DUnit a ~ u) => Scale (Image u a) where
  scale sx sy = fmap (scale sx sy)


instance (Num u, Translate a, DUnit a ~ u) => Translate (Image u a) where
  translate dx dy = fmap (translate dx dy)


instance (Real u, Floating u, Rotate a, DUnit a ~ u) => 
    Rotate (LocImage u a) where
  rotate ang = fmap (rotate ang)

instance (Real u, Floating u, RotateAbout a, DUnit a ~ u) => 
    RotateAbout (LocImage u a) where
  rotateAbout ang pt = fmap (rotateAbout ang pt)


instance (Num u, Scale a, DUnit a ~ u) => Scale (LocImage u a) where
  scale sx sy = fmap (scale sx sy)


instance (Num u, Translate a, DUnit a ~ u) => Translate (LocImage u a) where
  translate dx dy = fmap (translate dx dy)

--------------------------------------------------------------------------------


hyperlink :: XLink -> Image u a -> Image u a
hyperlink hypl = 
    fmap (\(a,prim) -> (a, metamorphPrim (xlink hypl) prim))






