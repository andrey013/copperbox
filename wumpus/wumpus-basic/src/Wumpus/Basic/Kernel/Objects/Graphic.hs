{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Graphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Graphic type - this is largely equivalent to Primitive in
-- Wumpus-Core, but drawing attributes are implicitly supplied 
-- by the DrawingContext.
--
-- API in @Wumpus.Core@, but here they exploit the implicit 
-- @DrawingContext@.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Graphic
  (

    Graphic
  , DGraphic

  , runGraphic


  , fontDeltaGraphic
  , xlinkGraphic
  , openStroke
  , closedStroke
  , filledPath
  , borderedPath


  , straightLineBetween
  , curveBetween

  ) where

import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive

import Wumpus.Core                              -- package: wumpus-core



--------------------------------------------------------------------------------
-- Graphic

-- | Simple drawing - produce a primitive, access the DrawingContext
-- if required.
--
type Graphic u      = CF (PrimGraphic u)

type DGraphic           = Graphic Double

type instance DUnit (Graphic u) = u



runGraphic :: DrawingContext -> Graphic u -> PrimGraphic u
runGraphic ctx df = runCF ctx df


-- Affine instances

instance (Real u, Floating u) => Rotate (Graphic u) where
  rotate ang = postpro (rotate ang) 


instance (Real u, Floating u) => RotateAbout (Graphic u) where
  rotateAbout ang pt = postpro (rotateAbout ang pt)


instance Num u => Scale (Graphic u) where
  scale sx sy = postpro (scale sx sy)


instance Num u => Translate (Graphic u) where
  translate dx dy = postpro (translate dx dy)


-- drawGraphic :: (Real u, Floating u, FromPtSize u) 
--             => DrawingContext -> Graphic u -> Picture u
-- drawGraphic ctx gf = frame [getPrimGraphic $ runGraphic ctx gf]



fontDeltaGraphic :: Graphic u -> Graphic u
fontDeltaGraphic df = 
    drawingCtx `bind` \ctx -> postpro (fun $ font_props ctx) df
  where 
    fun attr = metamorphPrim (fontDeltaContext attr)


xlinkGraphic :: XLink -> Graphic u -> Graphic u
xlinkGraphic hypl = postpro (metamorphPrim (xlink hypl))



openStroke :: Num u => PrimPath u -> Graphic u
openStroke pp = 
    withStrokeAttr $ \rgb attr -> primGraphic $ ostroke rgb attr pp

closedStroke :: Num u => PrimPath u -> Graphic u
closedStroke pp = 
    withStrokeAttr $ \rgb attr -> primGraphic $ cstroke rgb attr pp

filledPath :: Num u => PrimPath u -> Graphic u
filledPath pp = withFillAttr $ \rgb -> primGraphic $ fill rgb pp
                 


borderedPath :: Num u => PrimPath u -> Graphic u
borderedPath pp =
    withBorderedAttr $ \frgb attr srgb -> 
                           primGraphic $ fillStroke frgb attr srgb pp


-- Note - clipping needs a picture as well as a path, so there is
-- no analogous @clippedPath@ function.

--------------------------------------------------------------------------------

          

straightLineBetween :: Fractional u => Point2 u -> Point2 u -> Graphic u
straightLineBetween p1 p2 = openStroke $ primPath p1 [lineTo p2]



curveBetween :: Fractional u 
             => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Graphic u
curveBetween sp cp1 cp2 ep = openStroke $ primPath sp [curveTo cp1 cp2 ep]




