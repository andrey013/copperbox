{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Base
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


    Shape
  , DShape

  , makeShape
  , strokedShape
  , filledShape
  , borderedShape
  , rstrokedShape
  , rfilledShape
  , rborderedShape

  , roundCornerShapePath

  , updateAngle
  , setDecoration

  , ShapeCTM
  , makeShapeCTM
  , ctmCenter
  , ctmAngle
  , projectPoint

 

  ) where

import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative


-- | Shape is a newtype wrapper over a /LocTheta/ function - 
-- /a function from Point and Angle to Answer/. 
--
-- The answer is a pair of some polymorphic type @a@ and a path.
-- When the Shape is drawn, the rendering function 
-- (@strokedShape@, etc.) uses the path for drawing and returns 
-- the polymorphic answer @a@. 
--
-- The @a@ will represent some concrete shape object (Rectangle, 
-- Triangle etc.). Crucial for shape objects is that they support
-- Anchors - this allows connectors to address specific locations
-- on the shape border so \"node and link\" diagrams can be made 
-- easily.
--
data Shape u a = Shape 
      { shape_loc_fun     :: LocThetaCF u (a, Path u) 
      , shape_decoration  :: LocThetaGraphic u
      }

--
-- Design note - are shapes really LocThetaCF? - YES
--
-- This would remove the need for r__ versions and it means
-- decorations (i.e. text labels) would be simpler.
-- 
-- There could be r__ versions of the filledShape, etc. as there
-- are only three so far so that would make 6 in total.
--

type DShape a = Shape Double a

type instance DUnit (Shape u a) = u


instance Functor (Shape u) where
  fmap f = (\s i -> s { shape_loc_fun = fmap (bimapL f) i }) 
             <*> shape_loc_fun



makeShape :: Num u => LocThetaCF u a -> LocThetaCF u (Path u) -> Shape u a
makeShape f g = Shape { shape_loc_fun    = liftA2 (,) f g
                      , shape_decoration = emptyLocThetaGraphic
                      }

-- for Wumpus-Basic...
emptyLocThetaGraphic :: Num u => LocThetaGraphic u
emptyLocThetaGraphic = lift1R2 emptyLocGraphic 



strokedShape :: Num u => Shape u a -> LocImage u a
strokedShape = shapeToLoc closedStroke


filledShape :: Num u => Shape u a -> LocImage u a
filledShape = shapeToLoc filledPath


borderedShape :: Num u => Shape u a -> LocImage u a
borderedShape = shapeToLoc borderedPath


shapeToLoc :: Num u 
           => (PrimPath u -> Graphic u) -> Shape u a -> LocImage u a
shapeToLoc pathF sh = promoteR1 $ \pt -> 
    atRot (shape_loc_fun sh) pt 0 >>= \(a,spath) -> 
    let g1 = pathF $ toPrimPath spath 
        g2 = atRot (shape_decoration sh) pt 0 
    in intoImage (pure a) (g1 `oplus` g2)

{-
-- for Wumpus-Basic?
--
decorate :: Image u a -> Graphic u -> Image u a
decorate mf mg = 
   mf >>= \(a,g1) -> mg >>= \(_,g2) -> return (a,g1 `oplus` g2)
-}


rstrokedShape :: Num u => Shape u a -> LocThetaImage u a
rstrokedShape = shapeToLocTheta closedStroke


rfilledShape :: Num u => Shape u a -> LocThetaImage u a
rfilledShape = shapeToLocTheta filledPath


rborderedShape :: Num u => Shape u a -> LocThetaImage u a
rborderedShape = shapeToLocTheta borderedPath


shapeToLocTheta :: Num u 
                => (PrimPath u -> Graphic u) -> Shape u a -> LocThetaImage u a
shapeToLocTheta pathF sh = promoteR2 $ \pt theta -> 
    atRot (shape_loc_fun sh) pt theta >>= \(a,spath) -> 
    let g1 = pathF $ toPrimPath spath 
        g2 = atRot (shape_decoration sh) pt theta
    in intoImage (pure a) (g1 `oplus` g2)



-- | Draw the shape path with round corners.
-- 
roundCornerShapePath :: (Real u, Floating u, FromPtSize u) 
                     => [Point2 u] -> CF (Path u)
roundCornerShapePath xs = getRoundCornerSize >>= \sz -> 
    if sz == 0 then return (traceLinePoints xs) 
               else return (roundTrail  sz xs)

updateAngle :: (Radian -> Radian) -> Shape u a -> Shape u a
updateAngle f = (\s i -> s { shape_loc_fun = moveTheta (circularModulo . f) i})
                 <*> shape_loc_fun

setDecoration :: LocThetaGraphic u -> Shape u a -> Shape u a
setDecoration gf = (\s -> s { shape_decoration = gf })


-- | Move the start-point of a LocImage with the supplied 
-- displacement function.
--
moveTheta :: (Radian -> Radian) -> LocThetaCF u a -> LocThetaCF u a
moveTheta f ma = promoteR2 $ \pt theta -> apply2R2 ma pt (f theta)



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

makeShapeCTM :: Num u => Point2 u -> Radian -> ShapeCTM u
makeShapeCTM pt ang = ShapeCTM { ctm_center   = pt
                               , ctm_scale_x  = 1
                               , ctm_scale_y  = 1
                               , ctm_rotation = ang }


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

