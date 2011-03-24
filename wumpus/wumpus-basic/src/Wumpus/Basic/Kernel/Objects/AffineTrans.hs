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

  , uniformScale
  , reflectX
  , reflectY
  , translateBy
  , reflectXPlane
  , reflectYPlane
  
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
-- Common rotations



-- | Rotate by 30 degrees about the origin. 
--
rotate30 :: (CtxRotate t u, InterpretUnit u)
         => Image t u -> Image t u
rotate30 = rotate (pi/6) 

-- | Rotate by 30 degrees about the supplied point.
--
rotate30About :: (CtxRotateAbout t u, InterpretUnit u)
              => Point2 u -> Image t u -> Image t u
rotate30About = rotateAbout (pi/6) 

-- | Rotate by 45 degrees about the origin. 
--
rotate45 :: (CtxRotate t u, InterpretUnit u)
         => Image t u -> Image t u
rotate45 = rotate (pi/4) 

-- | Rotate by 45 degrees about the supplied point.
--
rotate45About :: (CtxRotateAbout t u, InterpretUnit u)
              => Point2 u -> Image t u -> Image t u
rotate45About = rotateAbout (pi/4)

-- | Rotate by 60 degrees about the origin. 
--
rotate60 :: (CtxRotate t u, InterpretUnit u)
         => Image t u -> Image t u
rotate60 = rotate (2*pi/3) 

-- | Rotate by 60 degrees about the supplied point.
--
rotate60About :: (CtxRotateAbout t u, InterpretUnit u)
              => Point2 u -> Image t u -> Image t u
rotate60About = rotateAbout (2*pi/3)

-- | Rotate by 90 degrees about the origin. 
--
rotate90 :: (CtxRotate t u, InterpretUnit u)
         => Image t u -> Image t u
rotate90 = rotate (pi/2) 

-- | Rotate by 90 degrees about the supplied point.
--
rotate90About :: (CtxRotateAbout t u, InterpretUnit u)
              => Point2 u -> Image t u -> Image t u
rotate90About = rotateAbout (pi/2)

-- | Rotate by 120 degrees about the origin. 
--
rotate120 :: (CtxRotate t u, InterpretUnit u)
          => Image t u -> Image t u
rotate120 = rotate (4*pi/3) 

-- | Rotate by 120 degrees about the supplied point.
--
rotate120About :: (CtxRotateAbout t u, InterpretUnit u)
               => Point2 u -> Image t u -> Image t u
rotate120About = rotateAbout (4*pi/3)



--------------------------------------------------------------------------------
-- Common scalings

-- | Scale both x and y dimensions by the same amount.
--
uniformScale :: (CtxScale t u, InterpretUnit u) 
             => Double -> Image t u -> Image t u
uniformScale a = scale a a 

-- | Reflect in the X-plane about the origin.
--
reflectX :: (CtxScale t u, InterpretUnit u) 
         => Image t u -> Image t u
reflectX = scale (-1) 1

-- | Reflect in the Y-plane about the origin.
--
reflectY :: (CtxScale t u, InterpretUnit u) 
         => Image t u -> Image t u
reflectY = scale 1 (-1)

--------------------------------------------------------------------------------
-- Translations

-- | Translate by the x and y components of a vector.
--
translateBy :: (CtxTranslate t u, InterpretUnit u) 
            => Vec2 u -> Image t u -> Image t u
translateBy (V2 x y) = translate x y


--------------------------------------------------------------------------------
-- Translation and scaling

-- | Reflect in the X plane that intersects the supplied point. 
--
reflectXPlane :: (CtxTranslate t u, CtxScale t u, InterpretUnit u) 
              => Point2 u -> Image t u -> Image t u
reflectXPlane (P2 x y) = translate x y . scale (-1) 1 . translate (-x) (-y)

-- | Reflect in the Y plane that intersects the supplied point.
--
reflectYPlane :: (CtxTranslate t u, CtxScale t u, InterpretUnit u) 
              => Point2 u -> Image t u -> Image t u
reflectYPlane (P2 x y) = translate x y . scale 1 (-1) . translate (-x) (-y)


--------------------------------------------------------------------------------


-- affine trans

-- Note - ImageAns needs more than Functor access to transform
-- its body (it needs to operate on the CatPrim). Thus we are 
-- burdened with the Ctx family of classes.
-- 

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

