{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.AffineTrans
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Affine transformations...
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.AffineTrans
  (
    Rotate(..)
  , CtxRotate(..)

  -- * Rotate about
  , RotateAbout(..)
  , CtxRotateAbout(..)

  -- * Scale
  , Scale(..)
  , CtxScale(..)
  
  -- * Translate
  , Translate(..)
  , CtxTranslate(..)

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs

import Wumpus.Core                              -- package: wumpus-core


--------------------------------------------------------------------------------


-- affine trans

class Rotate o where
  rotate :: Radian -> o -> o

class CtxRotate t u where
  ctxRotate :: FontSize -> Radian -> t u -> t u

-- Rotate About

class RotateAbout o u where
  rotateAbout :: DUnit o ~ u => Radian -> Point2 u -> o -> o

class CtxRotateAbout t u where
  ctxRotateAbout :: FontSize -> Radian -> Point2 u -> t u -> t u


-- Scale

class Scale o where
  scale :: Double -> Double -> o -> o

class CtxScale t u where
  ctxScale :: FontSize -> Double -> Double -> t u -> t u


-- Translate

class Translate o u where
  translate :: DUnit o ~ u => u -> u -> o -> o

class CtxTranslate t u where
  ctxTranslate :: FontSize -> u -> u -> t u -> t u


--------------------------------------------------------------------------------

-- Point2 

instance InterpretUnit u => CtxRotate Point2 u where
  ctxRotate sz ang = intraMapFunctor sz (drotate ang)

instance InterpretUnit u => CtxRotateAbout Point2 u where
  ctxRotateAbout sz ang p0 = 
      intraMapFunctor sz (drotateAbout ang (normalizeF sz p0))

instance InterpretUnit u => CtxScale Point2 u where
  ctxScale sz sx sy = intraMapFunctor sz (dscale sx sy)

instance InterpretUnit u => CtxTranslate Point2 u where
  ctxTranslate sz dx dy = 
      intraMapFunctor sz (dscale (normalize sz dx) (normalize sz dy))

-- Vec2 

instance InterpretUnit u => CtxRotate Vec2 u where
  ctxRotate sz ang = intraMapFunctor sz (drotate ang)

instance InterpretUnit u => CtxRotateAbout Vec2 u where
  ctxRotateAbout sz ang p0 = 
      intraMapFunctor sz (drotateAbout ang (normalizeF sz p0))

instance InterpretUnit u => CtxScale Vec2 u where
  ctxScale sz sx sy = intraMapFunctor sz (dscale sx sy)

instance InterpretUnit u => CtxTranslate Vec2 u where
  ctxTranslate sz dx dy = 
      intraMapFunctor sz (dscale (normalize sz dx) (normalize sz dy))
              

-- BoundingBox

instance InterpretUnit u => CtxRotate BoundingBox u where
  ctxRotate sz ang = intraMapFunctor sz (drotate ang)

instance InterpretUnit u => CtxRotateAbout BoundingBox u where
  ctxRotateAbout sz ang p0 = 
      intraMapFunctor sz (drotateAbout ang (normalizeF sz p0))

instance InterpretUnit u => CtxScale BoundingBox u where
  ctxScale sz sx sy = intraMapFunctor sz (dscale sx sy)

instance InterpretUnit u => CtxTranslate BoundingBox u where
  ctxTranslate sz dx dy = 
      intraMapFunctor sz (dscale (normalize sz dx) (normalize sz dy))



-- UNil

instance CtxRotate UNil u where
  ctxRotate _ _ = id


instance CtxScale UNil u where
  ctxScale _ _ _ = id 


instance CtxRotateAbout UNil u where
  ctxRotateAbout _ _ _ = id


instance CtxTranslate UNil u where
  ctxTranslate _ _ _ = id 

