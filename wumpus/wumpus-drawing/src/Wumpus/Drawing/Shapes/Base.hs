{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
  , projectFromCtr

  ) where

import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( white )

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative

-- import Data.Traversable ( Traversable )
-- import qualified Data.Traversable as T

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
      { shape_ans_fun     :: LocThetaQuery u (t u)
      , shape_path_fun    :: LocThetaQuery u (AbsPath u) 
      , shape_decoration  :: LocThetaGraphic u
      }


type DShape t = Shape t Double



-- Design note - shapes look like they should be constrained to 
-- InterpretUnit rather than CtxSize, this allows direct affine 
-- transformations.
--

--------------------------------------------------------------------------------

shapeMap :: (t u -> t' u) -> Shape t u -> Shape t' u
shapeMap f = (\s sf -> s { shape_ans_fun = promoteR2 $ \pt ang -> 
                                           fmap f $ apply2R2 sf pt ang }) 
                <*> shape_ans_fun


-- Note - there are no instances of Applicative, Monad, 
-- DrawingCtxM... so Shapes cannot have localized drawing props.
--
-- @localize@ must be performed in the context of @strokeShape@, 
-- @fillShape@ etc.
--



--------------------------------------------------------------------------------


makeShape :: InterpretUnit u
          => LocThetaQuery u (t u) -> LocThetaQuery u (AbsPath u) -> Shape t u
makeShape f g = Shape { shape_ans_fun    = f
                      , shape_path_fun   = g
                      , shape_decoration = emptyLocThetaGraphic
                      }




strokedShape :: InterpretUnit u => Shape t u -> LocImage t u
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
dblStrokedShape :: InterpretUnit u => Shape t u -> LocImage t u
dblStrokedShape sh = decorate back fore 
  where
    img  = shapeToLoc closedStroke sh
    back = getLineWidth >>= \lw ->
           localize (set_line_width $ lw * 3.0) img
    fore = ignoreAns $ localize (stroke_colour white) img



filledShape :: InterpretUnit u => Shape t u -> LocImage t u
filledShape = shapeToLoc filledPath


borderedShape :: InterpretUnit u => Shape t u -> LocImage t u
borderedShape = shapeToLoc borderedPath


shapeToLoc :: InterpretUnit u
           => (PrimPath -> Graphic u) -> Shape t u -> LocImage t u
shapeToLoc drawF sh = promoteR1 $ \pt -> 
    apply2R2 (shape_ans_fun sh)  pt 0 >>= \a -> 
    apply2R2 (shape_path_fun sh) pt 0 >>= \spath -> 
    let g2 = atIncline (shape_decoration sh) pt 0 
    in intoImage (pure a) (decorate g2 $ toPrimPath spath >>= drawF)



rstrokedShape :: InterpretUnit u => Shape t u -> LocThetaImage t u
rstrokedShape = shapeToLocTheta closedStroke


rfilledShape :: InterpretUnit u => Shape t u -> LocThetaImage t u
rfilledShape = shapeToLocTheta filledPath


rborderedShape :: InterpretUnit u => Shape t u -> LocThetaImage t u
rborderedShape = shapeToLocTheta borderedPath


shapeToLocTheta :: InterpretUnit u
                => (PrimPath -> Graphic u) -> Shape t u -> LocThetaImage t u
shapeToLocTheta drawF sh = promoteR2 $ \pt theta -> 
    apply2R2 (shape_ans_fun sh)  pt theta >>= \a -> 
    apply2R2 (shape_path_fun sh) pt theta >>= \spath -> 
    let g2 = atIncline (shape_decoration sh) pt theta
    in intoImage (pure a) (decorate g2 $ toPrimPath spath >>= drawF)



-- | Draw the shape path with round corners.
-- 
roundCornerShapePath :: (Real u, Floating u, InterpretUnit u, LengthTolerance u)
                     => [Point2 u] -> Query (AbsPath u)
roundCornerShapePath xs = 
    roundCornerSize >>= \sz -> 
    if sz == 0 then return (vertexPath xs) 
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
    (\s i -> s { shape_path_fun = promoteR2 $ \pt ang -> 
                                  apply2R2 i pt (mvTheta ang) })
      <*> shape_path_fun
  where
    mvTheta = circularModulo . f 

setDecoration :: LocThetaGraphic u -> Shape t u -> Shape t u
setDecoration gf = (\s -> s { shape_decoration = gf })



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

type instance DUnit (ShapeCTM u) = u


instance Functor ShapeCTM where
  fmap f = (\s i -> s { ctm_center = fmap f i }) <*> ctm_center


makeShapeCTM :: Point2 u -> Radian -> ShapeCTM u
makeShapeCTM pt ang = ShapeCTM { ctm_center   = pt
                               , ctm_scale_x  = 1
                               , ctm_scale_y  = 1
                               , ctm_rotation = ang }


ctmCenter :: ShapeCTM u -> Point2 u
ctmCenter = ctm_center

ctmAngle :: ShapeCTM u -> Radian
ctmAngle = ctm_rotation


instance (Fractional u) => Scale (ShapeCTM u) where
  scale sx sy = (\s x y pt -> s { ctm_scale_x = x*sx
                                , ctm_scale_y = y*sy 
                                , ctm_center  = scale sx sy pt })
                    <*> ctm_scale_x <*> ctm_scale_y <*> ctm_center


instance (Real u, Floating u) => Rotate (ShapeCTM u) where
  rotate ang = (\s i pt -> let ctr = rotate ang pt
                           in s { ctm_rotation = circularModulo $ i+ang
                                , ctm_center   = ctr })
                    <*> ctm_rotation <*> ctm_center


instance (Real u, Floating u) => RotateAbout (ShapeCTM u) where
  rotateAbout ang pt = 
    (\s ctr i -> s { ctm_rotation = circularModulo $ i+ang
                   , ctm_center   = rotateAbout ang pt ctr })
      <*> ctm_center <*> ctm_rotation


instance (Num u) => Translate (ShapeCTM u) where
  translate dx dy = 
    (\s i -> s { ctm_center = translate dx dy i })
      <*> ctm_center



projectFromCtr :: (Real u, Floating u) => Vec2 u -> ShapeCTM u -> Anchor u
projectFromCtr v (ShapeCTM { ctm_center   = ctr
                           , ctm_scale_x  = sx
                           , ctm_scale_y  = sy
                           , ctm_rotation = theta }) = 
     let v1 = rotate theta $ scale sx sy $ v in ctr .+^ v1


