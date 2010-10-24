{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Shapes.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common core for shapes
-- 
-- \*\* WARNING \*\* - the types of Shapes and Plaintext are not
-- ideal and are pending revision.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes.Base
  ( 


    Shape
  , LocShape
  , makeShape

  , ShapeConstructor

  , borderedShape
  , filledShape
  , strokedShape

  -- * ShapeCTM 
  , ShapeCTM
  , makeShapeCTM

  , ShapeGeom
  , runShapeGeom
  , askCTM
  , projectPoint
  , shapeCenter
  , shapeAngle

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative


-- Currently shapes that aren\'t paths:
--
-- > Coordinate
-- > FreeLabel
--
-- Alternative 
--
-- > out_fun :: ShapeCTM u -> (Path u,sh)
--
-- All shapes expect FreeLabel are oblivious to the 
-- DrawingContext for the /shape/
--

newtype ShapeR u a = ShapeR { getShapeR :: ShapeCTM u -> a }

runShapeR :: ShapeCTM u -> ShapeR u a -> a
runShapeR ctm sf = getShapeR sf ctm


data Shape u t =  Shape 
      { shape_ctm   :: ShapeCTM u 
      , path_fun    :: ShapeR u (Path u)
      , cons_fun    :: ShapeR u (t u)
      }

type instance DUnit (Shape u sh) = u


type LocShape u t = Point2 u -> Shape u t

type ShapeConstructor u t = ShapeCTM u -> t u 


makeShape :: Num u 
          => (ShapeCTM u -> Path u) -> (ShapeCTM u -> t u) -> LocShape u t
makeShape pf mkf = \pt -> Shape { shape_ctm = makeShapeCTM pt
                                , path_fun  = ShapeR pf
                                , cons_fun  = ShapeR mkf
                                } 


shapeImage :: Num u => (PrimPath u -> Graphic u) -> Shape u t -> Image u (t u)
shapeImage drawF (Shape { shape_ctm = ctm, path_fun = pf, cons_fun = objf }) = 
   intoImage (pure $ runShapeR ctm objf) 
             (drawF $ toPrimPath $ runShapeR ctm pf)


borderedShape :: Num u => Shape u t -> Image u (t u)
borderedShape = shapeImage borderedPath

filledShape :: Num u => Shape u t -> Image u (t u)
filledShape = shapeImage filledPath

strokedShape :: Num u => Shape u t -> Image u (t u)
strokedShape = shapeImage closedStroke 



instance (Real u, Floating u) => Rotate (Shape u sh) where
  rotate r = updateCTM (rotate r)

instance (Real u, Floating u) => RotateAbout (Shape u sh) where
  rotateAbout r pt = updateCTM (rotateAbout r pt)

instance Num u => Scale (Shape u sh) where
  scale sx sy = updateCTM (scale sx sy)

instance Num u => Translate (Shape u sh) where
  translate dx dy = updateCTM (translate dx dy)


updateCTM :: (ShapeCTM u -> ShapeCTM u) -> Shape u sh -> Shape u sh
updateCTM fn (Shape ctm pf mkf) = Shape (fn ctm) pf mkf

--------------------------------------------------------------------------------
-- CTM

-- Note - all shapes need a location (usually/always the center)
-- so this needs to be stored in the CTM.
--

data ShapeCTM u = ShapeCTM 
      { ctm_center              :: Point2 u
      , ctm_scale_x             :: !u
      , ctm_scale_y             :: !u
      , ctm_rotation            :: Radian
      }
  deriving (Eq,Ord,Show)


type instance DUnit (ShapeCTM u) = u

makeShapeCTM :: Num u => Point2 u -> ShapeCTM u
makeShapeCTM pt = ShapeCTM { ctm_center   = pt
                           , ctm_scale_x  = 1
                           , ctm_scale_y  = 1
                           , ctm_rotation = 0 }





instance Num u => Scale (ShapeCTM u) where
  scale sx sy = (\s x y -> s { ctm_scale_x = x*sx, ctm_scale_y = y*sy })
                  <*> ctm_scale_x <*> ctm_scale_y


instance Rotate (ShapeCTM u) where
  rotate ang = (\s i -> s { ctm_rotation = circularModulo $ i+ang })
                  <*> ctm_rotation

instance (Real u, Floating u) => RotateAbout (ShapeCTM u) where
  rotateAbout ang pt = 
    (\s ctr i -> s { ctm_rotation = circularModulo $ i+ang
                   , ctm_center   = rotateAbout ang pt ctr })
      <*> ctm_center <*> ctm_rotation


instance Num u => Translate (ShapeCTM u) where
  translate dx dy = (\s (P2 x y) -> s { ctm_center = P2 (x+dx) (y+dy) })
                      <*> ctm_center


--------------------------------------------------------------------------------

newtype ShapeGeom u a = ShapeGeom { getShapeGeom :: ShapeCTM u -> a }


type instance MonUnit (ShapeGeom u) = u


instance Functor (ShapeGeom u) where
  fmap f ma = ShapeGeom $ \ctx -> let a = getShapeGeom ma ctx in f a

instance Applicative (ShapeGeom u) where
  pure a    = ShapeGeom $ \_   -> a
  mf <*> ma = ShapeGeom $ \ctx -> let f = getShapeGeom mf ctx
                                      a = getShapeGeom ma ctx
             			  in (f a)

instance Monad (ShapeGeom u) where
  return a = ShapeGeom $ \_   -> a
  m >>= k  = ShapeGeom $ \ctx -> let a = getShapeGeom m ctx
    	     	                 in (getShapeGeom . k) a ctx



runShapeGeom :: ShapeCTM u -> ShapeGeom u a -> a
runShapeGeom ctm mf = getShapeGeom mf ctm

askCTM :: ShapeGeom u (ShapeCTM u)
askCTM = ShapeGeom $ \ctm -> ctm

shapeCenter :: ShapeGeom u (Point2 u)
shapeCenter = ShapeGeom $ \ctm -> ctm_center ctm

shapeAngle :: ShapeGeom u Radian
shapeAngle = ShapeGeom $ \ctm -> ctm_rotation ctm



projectPoint :: (Real u, Floating u) => Point2 u -> ShapeGeom u (Point2 u)
projectPoint (P2 x y) = ShapeGeom $ 
    \(ShapeCTM { ctm_center   = (P2 dx dy)
               , ctm_scale_x  = sx
               , ctm_scale_y  = sy
               , ctm_rotation = theta }) -> 
    translate dx dy $ rotate theta $ P2 (sx*x) (sy*y)

