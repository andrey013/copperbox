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

module Wumpus.Basic.Kernel.Objects.AffineTrans
  (
   
    CtxRotate(..)
  , CtxRotateAbout(..)
  , CtxScale(..)
  , CtxTranslate(..)

  , rotate
  , rotateAbout
  , scale
  , translate

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image

import Wumpus.Core                              -- package: wumpus-core


--------------------------------------------------------------------------------


-- affine trans


class CtxRotate t u where
  ctxRotate :: FontSize -> Radian -> t u -> t u

-- Rotate About


class CtxRotateAbout t u where
  ctxRotateAbout :: FontSize -> Radian -> Point2 u -> t u -> t u


-- Scale


class CtxScale t u where
  ctxScale :: FontSize -> Double -> Double -> t u -> t u


-- Translate


class CtxTranslate t u where
  ctxTranslate :: FontSize -> u -> u -> t u -> t u





affineTransImg :: InterpretUnit u
              => (FontSize -> ImageAns t u -> ImageAns t u)
              -> Image t u
              -> Image t u
affineTransImg fn df = drawingCtx >>= \ctx -> 
    let sz  = dc_font_size ctx in return $ fn sz $ runCF df ctx





rotate :: (CtxRotate t u, InterpretUnit u) 
       => Radian -> Image t u -> Image t u
rotate ang          = affineTransImg (\sz -> ctxRotate sz ang)

rotateAbout :: (CtxRotateAbout t u, InterpretUnit u) 
            => Radian -> Point2 u -> Image t u -> Image t u
rotateAbout ang p0  = affineTransImg (\sz -> ctxRotateAbout sz ang p0)

scale :: (CtxScale t u, InterpretUnit u) 
      => Double -> Double -> Image t u -> Image t u
scale sx sy         = affineTransImg (\sz -> ctxScale sz sx sy)

translate :: (CtxTranslate t u, InterpretUnit u) 
          => u -> u -> Image t u -> Image t u 
translate dx dy     = affineTransImg (\sz -> ctxTranslate sz dx dy)





--------------------------------------------------------------------------------


-- affine trans



instance (CtxRotate t u, InterpretUnit u) => 
    CtxRotate (ImageAns t) u where
  ctxRotate sz ang = trafoImageAns (ctxRotate sz ang) (drotate ang)


instance (CtxRotateAbout t u, InterpretUnit u) => 
    CtxRotateAbout (ImageAns t) u where
  ctxRotateAbout sz ang pt = let dpt = normalizeF sz pt 
                             in trafoImageAns (ctxRotateAbout sz ang pt) 
                                              (drotateAbout ang dpt)


instance (CtxScale t u, InterpretUnit u) => 
    CtxScale (ImageAns t) u where
  ctxScale sz sx sy = trafoImageAns (ctxScale sz sx sy) (dscale sx sy)


instance (CtxTranslate t u, InterpretUnit u) => 
    CtxTranslate (ImageAns t) u where
  ctxTranslate sz dx dy = let ddx = normalize sz dx
                              ddy = normalize sz dy
                          in trafoImageAns (ctxTranslate sz dx dy) 
                                           (dtranslate ddx ddy)






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

