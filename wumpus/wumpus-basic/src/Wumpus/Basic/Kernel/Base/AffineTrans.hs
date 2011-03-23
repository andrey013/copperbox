{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
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

class Rotate t u where
  rotate :: Radian -> t u -> t u

class CtxRotate t u where
  ctxRotate :: FontSize -> Radian -> t u -> t u


class RotateAbout t u where
  rotateAbout :: Radian -> Point2 u -> t u -> t u

class CtxRotateAbout t u where
  ctxRotateAbout :: FontSize -> Radian -> Point2 u -> t u -> t u


class Scale t u where
  scale :: Double -> Double -> t u -> t u

class CtxScale t u where
  ctxScale :: FontSize -> Double -> Double -> t u -> t u


class Translate t u where
  translate :: u -> u -> t u -> t u

class CtxTranslate t u where
  ctxTranslate :: FontSize -> u -> u -> t u -> t u


--------------------------------------------------------------------------------


instance InterpretUnit u => CtxRotate Point2 u where
  ctxRotate sz ang pt = uconvertF sz $ drotate ang $ normalizeF sz pt




instance CtxRotate UNil u where
  ctxRotate _ _ = id


instance CtxScale UNil u where
  ctxScale _ _ _ = id 


instance CtxRotateAbout UNil u where
  ctxRotateAbout _ _ _ = id


instance CtxTranslate UNil u where
  ctxTranslate _ _ _ = id 

