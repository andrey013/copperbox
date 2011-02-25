{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.BaseObjects
-- Copyright   :  (c) Stephen Tetley 2010-2011
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
type Image u a          = CF (ImageAns u a)


-- | Draw a PrimGraphic respective to the 'DrawingContext' and 
-- the supplied point, return some answer @a@.
-- 
type LocImage u a       = LocCF u (ImageAns u a)


-- | Draw a PrimGraphic respective to the 'DrawingContext' and
-- the supplied point and angle.
-- 
type LocThetaImage u a  = LocThetaCF u (ImageAns u a)



type DImage a            = Image Double a
type DLocImage a         = LocImage Double a
type DLocThetaImage a    = LocThetaImage Double a


type instance DUnit (Image u a) = u



--------------------------------------------------------------------------------
-- Affine instances

-- Note - it seems better to have these instances for Image (even 
-- though Image is a type synonym) rather than more general 
-- instances on a CF.
--
-- There is nothing determining a DUnit for the CF types.
--
-- The downside is these instances are effectively orphan 
-- instances.
--

instance (Real u, Floating u, PtSize u, Rotate a) => 
    Rotate (Image u a) where
  rotate ang = fmap (rotate ang)


instance (Real u, Floating u, PtSize u, RotateAbout a, u ~ DUnit a) => 
    RotateAbout (Image u a) where
  rotateAbout ang pt = fmap (rotateAbout ang pt)


instance (Num u, PtSize u, Scale a) => Scale (Image u a) where
  scale sx sy = fmap (scale sx sy)


instance (Num u, PtSize u, Translate a) => Translate (Image u a) where
  translate dx dy = fmap (translate dx dy)


-- \*\* WARNING \*\* - I am not sure having affine instances for 
-- LocImage makes sense...
--
-- Particularly, what is a rotateAbout on a function from Point to 
-- Graphic? Is it just a post-transformation, or should the start 
-- point be transformed as well.
--

instance (Real u, Floating u, PtSize u, Rotate a) => 
    Rotate (LocImage u a) where
  rotate ang = fmap (rotate ang)

instance (Real u, Floating u, PtSize u, RotateAbout a, u ~ DUnit a) => 
    RotateAbout (LocImage u a) where
  rotateAbout ang pt = fmap (rotateAbout ang pt)


instance (Num u, Scale a, PtSize u) => Scale (LocImage u a) where
  scale sx sy = fmap (scale sx sy)


instance (Num u, Translate a, PtSize u) => Translate (LocImage u a) where
  translate dx dy = fmap (translate dx dy)

--------------------------------------------------------------------------------








