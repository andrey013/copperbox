{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Image
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - /drawing/ an Image produces both a
-- @PrimGraphic@ and some polymorphic answer (the name Image is 
-- an obvious pun).
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Image
  (

  -- * Image
    Image
  , DImage


  -- * Run functions
  , runImage

  , extrGraphic

  , fontDeltaImage
  , xlinkImage

  , intoImage


  ) where

import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                      -- package: wumpus-core



--------------------------------------------------------------------------------
-- Image

-- | Images return a value as well as drawing. A /node/ is a 
-- typical example - nodes are drawing but the also support 
-- taking anchor points.
--
type Image u a          = CF (a, PrimGraphic u)

type DImage a           = Image Double a

type instance DUnit (Image u a) = u




--------------------------------------------------------------------------------
-- Image instances

-- Affine instances


instance (Real u, Floating u, Rotate a, DUnit a ~ u) => 
    Rotate (Image u a) where
  rotate ang = postpro (\(a,b) -> (rotate ang a, rotate ang b))


instance (Real u, Floating u, RotateAbout a, DUnit a ~ u) => 
    RotateAbout (Image u a) where
  rotateAbout ang pt = 
      postpro (\(a,b) -> (rotateAbout ang pt a, rotateAbout ang pt b))


instance (Num u, Scale a, DUnit a ~ u) => Scale (Image u a) where
  scale sx sy = postpro (\(a,b) -> (scale sx sy a, scale sx sy b))


instance (Num u, Translate a, DUnit a ~ u) => Translate (Image u a) where
  translate dx dy = postpro (\(a,b) -> (translate dx dy a, translate dx dy b))




--------------------------------------------------------------------------------
-- Run functions








runImage :: DrawingContext -> Image u a -> (a, PrimGraphic u)
runImage ctx img = runCF ctx img





-------------------------------------------------------------------------------
-- Dropping /answers/


extrGraphic :: Image u a -> Graphic u
extrGraphic = postpro snd



--------------------------------------------------------------------------------




fontDeltaImage :: Image u a -> Image u a
fontDeltaImage df = 
    drawingCtx `bind` \ctx -> postpro (fun $ font_props ctx) df
  where 
    fun attr = \(a,prim) -> (a, metamorphPrim (fontDeltaContext attr) prim)



xlinkImage :: XLink -> Image u a -> Image u a
xlinkImage hypl = 
    postpro (\(a,prim) -> (a, metamorphPrim (xlink hypl) prim))





intoImage :: CF a -> Graphic u -> Image u a
intoImage = postcomb (,)



