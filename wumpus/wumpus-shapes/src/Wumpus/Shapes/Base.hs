{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Common core for shapes (anchors...)
-- 
--------------------------------------------------------------------------------

module Wumpus.Shapes.Base
  ( 


  -- * CTM 
    CTM(..)
  , identityCTM
  , scaleCTM
  , rotateCTM
  , translateCTM
  , ctmDisplace
  , ctmCenter

  -- * Shape label
  , ShapeLabel(..)
  , labelGraphic

  , Draw(..)

  ) where

import Wumpus.Shapes.Utils

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.Monads.Drawing


import Data.AffineSpace                 -- package: vector-space

import Control.Applicative


-- Note - Need to think how to handle multi-line labels...





--------------------------------------------------------------------------------
-- CTM

data CTM u = CTM 
      { ctm_pos                 :: Point2 u
      , ctm_scale_x             :: u
      , ctm_scale_y             :: u
      , ctm_rotation            :: Radian
      }
  deriving (Eq,Show)

type instance DUnit (CTM u) = u

identityCTM :: Num u => CTM u
identityCTM = CTM { ctm_pos      = P2 0 0 
                  , ctm_scale_x  = 1
                  , ctm_scale_y  = 1
                  , ctm_rotation = 0 }

scaleCTM :: Num u => u -> u -> CTM u -> CTM u
scaleCTM x1 y1 = star2 (\s x y -> s { ctm_scale_x = x1*x, ctm_scale_y = y1*y })
                       ctm_scale_x
                       ctm_scale_y

rotateCTM :: Radian -> CTM u -> CTM u
rotateCTM ang1 = star (\s ang -> s { ctm_rotation = circularModulo $ ang1+ang })
                      ctm_rotation

translateCTM :: Num u => u -> u -> CTM u -> CTM u
translateCTM x1 y1 = star (\s (P2 x y) -> s { ctm_pos = P2 (x+x1) (y+y1) })
                          ctm_pos


ctmDisplace :: (Real u, Floating u) => u -> u -> CTM u -> Point2 u
ctmDisplace x y ctm = translate cx cy $ rotate (ctm_rotation ctm) (P2 x' y')
  where
    x'        = liftA (*x) ctm_scale_x ctm
    y'        = liftA (*y) ctm_scale_y ctm
    P2 cx cy  = ctm_pos ctm


ctmCenter :: (Real u, Floating u) => CTM u -> Point2 u
ctmCenter = ctmDisplace 0 0


--------------------------------------------------------------------------------
-- Shape label

-- Old - can be handled with with DrawingAttr from Wumpus-Basic.
--
-- TODO - am I going to consider multi-line labels?

newtype ShapeLabel = ShapeLabel { getShapeLabel :: String }
  deriving (Eq,Show)



-- Note - labels are not scaled ....
--
labelGraphic :: (Real u, Floating u, FromPtSize u) 
             => ShapeLabel -> (RGBi,FontAttr) -> CTM u -> Graphic u
labelGraphic sl (rgb,attr) ctm =
    wrapG $ rotatePrimitive ang $ textlabel (rgb,attr) text pt
  where
    (ang,ctr)         = (ctm_rotation ctm, ctm_pos ctm)        
    text              = getShapeLabel sl
    font_sz           = font_size attr
    twidth            = fromPtSize $ textWidth  font_sz (length text)
    theight           = fromPtSize $ textHeight font_sz
    pt                = let p1 = ctr .-^ V2 (0.5 * twidth) (0.5 * theight)
                        in rotateAbout ang ctr p1


-- OLD - stroke fill (and clip) should be lifters to AGraphic.

-- Can all shapes (except coordinates) be stroked and filled?
-- Probably better to have separate type classes...


class Draw sh where
  draw :: (u ~ DUnit sh) => sh -> AGraphic (Point2 u) u sh

-- outline is a good synonym for stroke

{-
-- stroke okay, but fill has a name clash...
class Stroke sh where
  stroke :: (u ~ DUnit sh) => sh -> AGraphic u sh
-} 
