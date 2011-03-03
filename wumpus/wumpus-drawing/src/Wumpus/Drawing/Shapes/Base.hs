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

  , shapeMap
  , makeShape
  , strokedShape
  , dblStrokedShape
  , filledShape
  , borderedShape
  , rstrokedShape
  , rfilledShape
  , rborderedShape

  , roundCornerShapePath

  , updatePathAngle
  , setDecoration

  , ShapeCTM
  , makeShapeCTM
  , ctmCenter
  , ctmAngle
  , displaceCenter
 

  ) where

import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( white )

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative


-- | Shape is a record of three /LocTheta/ functions - 
-- functions /from Point and Angle to answer/. 
--
-- The @shape_path_fun@ returns a path. When the Shape is drawn, 
-- the rendering function (@strokedShape@, etc.) uses the path for 
-- drawing and returns the polymorphic answer @a@ of the 
-- @shape_ans_fun@. Lastly the @shape_decoration@ function can 
-- instantiated to add decoration (e.g. text) to the Shape as it 
-- is rendered.
--
-- The @a@ of the @shape_ans_fun@ represents some concrete shape 
-- object (e.g. a Rectangle, Triangle etc.). Crucial for shape 
-- objects is that they support Anchors - this allows connectors 
-- to address specific locations on the Shape border so 
-- \"node and link\" diagrams can be made easily.
--
data Shape t u = Shape 
      { shape_ans_fun     :: LocThetaCF u (t u)
      , shape_path_fun    :: LocThetaCF u (Path u) 
      , shape_decoration  :: LocThetaGraphic u
      }


type DShape t = Shape t Double



--------------------------------------------------------------------------------

shapeMap :: (t u -> t' u) -> Shape t u -> Shape t' u
shapeMap f = (\s i -> s { shape_ans_fun = fmap f i }) 
                <*> shape_ans_fun


-- Note - there are no instances of Applicative, Monad, 
-- DrawingCtxM... so Shapes cannot have localized drawing props.
--
-- @localize@ must be performed in the context of @strokeShape@, 
-- @fillShape@ etc.
--



--------------------------------------------------------------------------------


makeShape :: PtSize u => LocThetaCF u (t u) -> LocThetaCF u (Path u) -> Shape t u
makeShape f g = Shape { shape_ans_fun    = f
                      , shape_path_fun   = g
                      , shape_decoration = emptyLocThetaGraphic
                      }




strokedShape :: PtSize u => Shape t u -> LocImage t u
strokedShape = shapeToLoc closedStroke


-- | Note - this is simplistic double stroking - draw a background 
-- line with triple thickness and draw a white line on top.
--
-- I think this is what TikZ does, but it works better for TikZ 
-- where the extra thickness seems to be accounted for by the 
-- anchors. For Wumpus, arrows cut into the outside black line.
--
-- Probably Wumpus should calculate two paths instead.
--
dblStrokedShape :: PtSize u => Shape t u -> LocImage t u
dblStrokedShape sh = decorate back fore 
  where
    img  = shapeToLoc closedStroke sh
    back = getLineWidth >>= \lw -> localize (set_line_width $ lw * 3.0) img
    fore = ignoreAns $ localize (stroke_colour white) img



filledShape :: PtSize u => Shape t u -> LocImage t u
filledShape = shapeToLoc filledPath


borderedShape :: PtSize u => Shape t u -> LocImage t u
borderedShape = shapeToLoc borderedPath


shapeToLoc :: PtSize u 
           => (PrimPath -> Graphic u) -> Shape t u -> LocImage t u
shapeToLoc pathF sh = promoteR1 $ \pt -> 
    atRot (shape_ans_fun sh)  pt 0 >>= \a -> 
    atRot (shape_path_fun sh) pt 0 >>= \spath -> 
    let g1 = pathF $ toPrimPath spath 
        g2 = atRot (shape_decoration sh) pt 0 
    in intoImage (pure a) (g1 `oplus` g2)



rstrokedShape :: PtSize u => Shape t u -> LocThetaImage t u
rstrokedShape = shapeToLocTheta closedStroke


rfilledShape :: PtSize u => Shape t u -> LocThetaImage t u
rfilledShape = shapeToLocTheta filledPath


rborderedShape :: PtSize u => Shape t u -> LocThetaImage t u
rborderedShape = shapeToLocTheta borderedPath


shapeToLocTheta :: PtSize u 
                => (PrimPath -> Graphic u) -> Shape t u -> LocThetaImage t u
shapeToLocTheta pathF sh = promoteR2 $ \pt theta -> 
    atRot (shape_ans_fun sh)  pt theta >>= \a -> 
    atRot (shape_path_fun sh) pt theta >>= \spath -> 
    let g1 = pathF $ toPrimPath spath 
        g2 = atRot (shape_decoration sh) pt theta
    in intoImage (pure a) (g1 `oplus` g2)



-- | Draw the shape path with round corners.
-- 
roundCornerShapePath :: (Real u, Floating u, PtSize u) 
                     => [Point2 u] -> CF (Path u)
roundCornerShapePath xs = getRoundCornerSize >>= \sz -> 
    if sz == 0 then return (traceLinePoints xs) 
               else return (roundTrail  sz xs)

-- | The path angle can be modified. This allows /inverse/ 
-- versions of shapes (e.g. InvTriangle) to be made by
-- wrapping a base Shape but rotating the path prior to drawing 
-- it.
-- 
-- Only the Path needs rotating, the decoration takes the original 
-- angle. The anchors are typically implemented by rotating the 
-- correspoding anchor of the wrapped Shape about its center.
-- 
updatePathAngle :: (Radian -> Radian) -> Shape t u -> Shape t u
updatePathAngle f = 
    (\s i -> s { shape_path_fun = moveTheta (circularModulo . f) i})
      <*> shape_path_fun

setDecoration :: LocThetaGraphic u -> Shape t u -> Shape t u
setDecoration gf = (\s -> s { shape_decoration = gf })




-- For Wumpus-Basic...
-- | Move the /rotation/ of a LocThetaImage with the supplied 
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
      , ctm_scale_x             :: !Double
      , ctm_scale_y             :: !Double
      , ctm_rotation            :: Radian
      }
  deriving (Eq,Ord,Show)


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

instance (Real u, Floating u, PtSize u) => RotateAbout (ShapeCTM u) where
  rotateAbout ang pt = 
    (\s ctr i -> s { ctm_rotation = circularModulo $ i+ang
                   , ctm_center   = rotateAbout ang pt ctr })
      <*> ctm_center <*> ctm_rotation


instance PtSize u => Translate (ShapeCTM u) where
  translate dx dy = 
    (\s (P2 x y) -> s { ctm_center = P2 (x + dpoint dx) (y + dpoint dy) })
      <*> ctm_center




ctmCenter :: ShapeCTM u  -> Point2 u
ctmCenter = ctm_center

ctmAngle :: ShapeCTM u -> Radian
ctmAngle = ctm_rotation


displaceCenter :: (Real u, Floating u, PtSize u) 
               => Vec2 u -> ShapeCTM u -> Point2 u
displaceCenter v0 (ShapeCTM { ctm_center   = ctr0
                            , ctm_scale_x  = sx
                            , ctm_scale_y  = sy
                            , ctm_rotation = theta }) = ctr .+^ v
  where
    ctr = rotate theta $ scale sx sy ctr0
    v   = rotateAbout theta ctr $ scale sx sy v0
     
    
