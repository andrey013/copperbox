{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Base2
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common core for shapes
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Base
  ( 


    LocShape
  , DLocShape

  , intoLocShape
  , strokedShape
  , filledShape
  , borderedShape

  , roundCornerShapePath

  , ShapeCTM
  , makeShapeCTM
  , ctmCenter
  , ctmAngle
  , projectPoint

 

  ) where

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Paths

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative


type LocShape u a = LocCF u (a, Path u)

type DLocShape a = LocShape Double a


intoLocShape :: LocCF u a -> LocCF u (Path u) -> LocCF u (a,Path u)
intoLocShape = liftA2 (,)

strokedShape :: Num u => LocShape u a -> LocImage u a
strokedShape mf = 
   promoteR1 $ \pt -> 
     (mf `at` pt) >>= \(a,spath) -> 
     intoImage (pure a) (closedStroke $ toPrimPath spath)


filledShape :: Num u => LocShape u a -> LocImage u a
filledShape mf = 
   promoteR1 $ \pt -> 
     (mf `at` pt) >>= \(a,spath) -> 
     intoImage (pure a) (filledPath $ toPrimPath spath)


borderedShape :: Num u => LocShape u a -> LocImage u a
borderedShape mf = 
   promoteR1 $ \pt -> 
     (mf `at` pt) >>= \(a,spath) -> 
     intoImage (pure a) (borderedPath $ toPrimPath spath)

-- | Draw the shape path with round corners.
-- 
roundCornerShapePath :: (Real u, Floating u, FromPtSize u) 
                     => [Point2 u] -> CF (Path u)
roundCornerShapePath xs = getRoundCornerSize >>= \sz -> 
    if sz == 0 then return (traceLinePoints xs) 
               else return (roundTrail  sz xs)



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




ctmCenter :: ShapeCTM u  -> Point2 u
ctmCenter = ctm_center

ctmAngle :: ShapeCTM u -> Radian
ctmAngle = ctm_rotation



projectPoint :: (Real u, Floating u) => Point2 u -> ShapeCTM u  -> Point2 u
projectPoint (P2 x y) (ShapeCTM { ctm_center   = (P2 dx dy)
                                , ctm_scale_x  = sx
                                , ctm_scale_y  = sy
                                , ctm_rotation = theta     }) =
    translate dx dy $ rotate theta $ P2 (sx*x) (sy*y)

