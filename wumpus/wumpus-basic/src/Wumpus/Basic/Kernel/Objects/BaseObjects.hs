{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
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

  , noAns

  , imageAns
  , getImageAns
  , answer
  , imageOutput

  , ctxConverti

  , bimapImageAns

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

--------------------------------------------------------------------------------
-- DrawingInfo

-- | A query on the DrawingContext.
--
-- Alias for 'CF'.
-- 
type DrawingInfo a              = CF a


-- | A query on the DrawingContext respective to the supplied
--  point.
--
-- Alias for 'LocCF'.
-- 
type LocDrawingInfo u a         = LocCF u a


-- | A query on the DrawingContext respective to the supplied
--  point and angle.
--
-- Alias for 'LocCF'.
-- 
type LocThetaDrawingInfo u a    = LocThetaCF u a




--------------------------------------------------------------------------------
-- Image


-- | An Image always returns a pair of some polymorphic answer @a@
-- and a PrimGraphic.
--
-- Note a PrimGraphic cannot be empty.
-- 
newtype ImageAns t u            = ImageAns { getImageAns :: (t u, Primitive) }

type GraphicAns u               = ImageAns (Const ()) u


-- | Draw a PrimGraphic repsective to the 'DrawingContext' and 
-- return some answer @a@.
-- 
type Image t u                  = CF (ImageAns t u)


-- | Draw a PrimGraphic respective to the 'DrawingContext' and 
-- the supplied point, return some answer @a@.
-- 
type LocImage t u               = LocCF u (ImageAns t u)


-- | Draw a PrimGraphic respective to the 'DrawingContext' and
-- the supplied point and angle.
-- 
type LocThetaImage t u          = LocThetaCF u (ImageAns t u)


noAns :: Const () u
noAns = Const ()

imageAns :: t u -> Primitive -> ImageAns t u
imageAns a b = ImageAns (a,b)


answer :: ImageAns t u -> t u
answer = fst . getImageAns

imageOutput :: ImageAns t u -> Primitive
imageOutput = snd . getImageAns



-- helper u is a phantom for ImageAns so it is not a Bifunctor

bimapImageAns :: (t a -> t1 b) -> (Primitive -> Primitive) 
              -> ImageAns t a -> ImageAns t1 b
bimapImageAns f g = ImageAns . bimap f g . getImageAns


convertImageAns :: (Functor t, CtxSize u, PsDouble u1) 
                => FontSize -> ImageAns t u -> ImageAns t u1
convertImageAns sz = 
    ImageAns . bimapL (fmap (fromPsDouble . cfSize sz)) . getImageAns


ctxConverti :: (Functor t, Functor f, CtxSize u, PsDouble u1) 
            => FontSize -> f (ImageAns t u) -> f (ImageAns t u1)
ctxConverti sz = fmap (convertImageAns sz) 

--------------------------------------------------------------------------------
-- OPlus instance

instance OPlus (t u) => OPlus (ImageAns t u) where
  ImageAns (a,p1) `oplus` ImageAns (b,p2) = 
      ImageAns (a `oplus` b, p1 `oplus` p2)

--------------------------------------------------------------------------------
-- Affine instances

instance Rotate (t u) => Rotate (ImageAns t u) where
  rotate ang = bimapImageAns (rotate ang) (rotate ang)

instance RotateAbout (t u) => RotateAbout (ImageAns t u) where
  rotateAbout ang pt = bimapImageAns (rotateAbout ang pt) (rotateAbout ang pt)

instance Scale (t u) => Scale (ImageAns t u) where
  scale sx sy = bimapImageAns (scale sx sy) (scale sx sy)

instance Translate (t u) => Translate (ImageAns t u) where
  translate dx dy = bimapImageAns (translate dx dy) (translate dx dy)


-- Note - it seems better to have these instances for Image (even 
-- though Image is a type synonym) rather than more general 
-- instances on a CF.
--
-- There is nothing determining a DUnit for the CF types.
--
-- The downside is these instances are effectively orphan 
-- instances.
--

instance Rotate (t u) => Rotate (Image t u) where
  rotate ang = fmap (rotate ang)


instance RotateAbout (t u) => RotateAbout (Image t u) where
  rotateAbout ang pt = fmap (rotateAbout ang pt)


instance Scale (t u) => Scale (Image t u) where
  scale sx sy = fmap (scale sx sy)


instance Translate (t u) => Translate (Image t u) where
  translate dx dy = fmap (translate dx dy)


-- \*\* WARNING \*\* - I am not sure having affine instances for 
-- LocImage makes sense...
--
-- Particularly, what is a rotateAbout on a function from Point to 
-- Graphic? Is it just a post-transformation, or should the start 
-- point be transformed as well.
--

instance Rotate (t u) => Rotate (LocImage t u) where
  rotate ang = fmap (rotate ang)

instance RotateAbout (t u) => RotateAbout (LocImage t u) where
  rotateAbout ang pt = fmap (rotateAbout ang pt)


instance Scale (t u) => Scale (LocImage t u) where
  scale sx sy = fmap (scale sx sy)


instance Translate (t u) => Translate (LocImage t u) where
  translate dx dy = fmap (translate dx dy)

--------------------------------------------------------------------------------








