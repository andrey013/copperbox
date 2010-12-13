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
  , Image
  , LocImage
  , LocThetaImage

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

-- | Draw a PrimGraphic repsective to the 'DrawingContext' and 
-- return some answer @a@.
-- 
type Image u a      = CF (a, PrimGraphic u)


-- | Draw a PrimGraphic respective to the 'DrawingContext' and 
-- the supplied point, return some answer @a@.
-- 
type LocImage u a   = LocCF u (a, PrimGraphic u)


-- | Draw a PrimGraphic respective to the 'DrawingContext' and
-- the supplied point and angle.
-- 
type LocThetaImage u a   = LocThetaCF u (a, PrimGraphic u)





type instance DUnit (Image u a) = u -- GuardEq (DUnit a) (DUnit (PrimGraphic u))

type instance DUnit (LocImage u a) = u --  GuardEq (DUnit a) (DUnit (PrimGraphic u))

type instance DUnit (LocThetaImage u a) = u

--------------------------------------------------------------------------------
-- Affine instances

instance (Real u, Floating u, Rotate a, DUnit a ~ u) => 
    Rotate (Image u a) where
  rotate ang = postpro (rotate ang)


instance (Real u, Floating u, RotateAbout a, DUnit a ~ u) => 
    RotateAbout (Image u a) where
  rotateAbout ang pt = postpro (rotateAbout ang pt)


instance (Num u, Scale a, DUnit a ~ u) => Scale (Image u a) where
  scale sx sy = postpro (scale sx sy)


instance (Num u, Translate a, DUnit a ~ u) => Translate (Image u a) where
  translate dx dy = postpro (translate dx dy)


instance (Real u, Floating u, Rotate a, DUnit a ~ u) => 
    Rotate (LocImage u a) where
  rotate ang = postpro1 (rotate ang)

instance (Real u, Floating u, RotateAbout a, DUnit a ~ u) => 
    RotateAbout (LocImage u a) where
  rotateAbout ang pt = postpro1 (rotateAbout ang pt)


--------------------------------------------------------------------------------


hyperlink :: XLink -> Image u a -> Image u a
hyperlink hypl = 
    postpro (\(a,prim) -> (a, metamorphPrim (xlink hypl) prim))






