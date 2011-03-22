{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.LocThetaImage
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- LocThetaImage and LocThetaGraphic types - these are functional 
-- types from the DrawingContext, start point and angle of 
-- inclination to a graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocThetaImage
   (
     LocThetaGraphic
   , LocThetaImage

   , DLocThetaGraphic
   , DLocThetaImage

   , intoLocThetaImage
   , emptyLocThetaGraphic
   , uconvertLocThetaImg
   
   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative


-- | 'LocThetaImage' - function from DrawingContext, start point 
-- and inclination to a polymorphic /answer/ and a graphic 
-- /primitive/ (ImageAns).
--
-- The answer is expected to be a Functor.
--
type LocThetaImage t u  = LocThetaQuery u (ImageAns t u)


-- | LocThetaGraphic - function from DrawingContext, start point 
-- and inclination to a graphic /primitive/ (GraphicAns).
--
type LocThetaGraphic u  = LocThetaImage UNil u


-- | Type specialized version of 'LocThetaImage'.
--
type DLocThetaImage t   = LocThetaImage t Double

-- | Type specialized version of 'LocThetaGraphic'.
--
type DLocThetaGraphic   = LocThetaGraphic Double 




-- | 'intoLocThetaImage' : @ loc_theta_query * loc_theta_graphic -> LocThetaImage @
--
-- /LocTheta/ version of 'intoImage'. 
-- 
-- The 'LocThetaImage' is built as a function from an implicit 
-- start point and angle of inclination to the answer.
--
intoLocThetaImage :: LocThetaQuery u (t u) 
                  -> LocThetaGraphic u 
                  -> LocThetaImage t u
intoLocThetaImage = liftA2 (\a (Ans _ p) -> Ans a p)



-- | 'emptyLocThetaGraphic' : @ LocThetaGraphic @
--
-- Build an empty 'LocThetaGraphic' (i.e. a function 
-- /from Point and Inclination to Graphic/). 
-- 
-- The 'emptyLocThetaGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- minimum bounding box at the implicit start point.
-- 
emptyLocThetaGraphic :: InterpretUnit u => LocThetaGraphic u
emptyLocThetaGraphic = lift1R2 emptyLocGraphic


-- | Use this to convert both 'LocThetaImage' and 
-- 'LocThetaGraphic'.
--
uconvertLocThetaImg :: (InterpretUnit u, InterpretUnit u1, Functor t) 
                    => LocThetaImage t u -> LocThetaImage t u1
uconvertLocThetaImg = uconvertR2a


--------------------------------------------------------------------------------
-- affine trans

{-

instance (Rotate (t Double), Functor t, InterpretUnit u) => 
    Rotate (LocThetaImage t u) where
  rotate ang            = affineTransR2a (rotate ang) (rotate ang)

instance (RotateAbout (t Double), Functor t, InterpretUnit u) => 
    RotateAbout (LocThetaImage t u) where
  rotateAbout ang pt    = 
    affineTransR2a (rotateAbout ang pt) (rotateAbout ang pt)

instance (Scale (t Double), Functor t, InterpretUnit u) => 
    Scale (LocThetaImage t u) where
  scale sx sy           = affineTransR2a (scale sx sy) (scale sx sy)

instance (Translate (t Double), Functor t, InterpretUnit u) => 
    Translate (LocThetaImage t u) where
  translate dx dy       = affineTransR2a (translate dx dy) (translate dx dy)

-}
