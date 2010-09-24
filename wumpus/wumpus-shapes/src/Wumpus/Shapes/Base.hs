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

  , DrawShape(..)
  , OutlineShape(..)

  ) where

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic


import Control.Applicative


-- Note - Need to think how to handle multi-line labels...





--------------------------------------------------------------------------------
-- CTM

-- Note - all shapes need a location (usually/always the center)
-- so this needs to be stored in the CTM.
--

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
scaleCTM x1 y1 = (\s x y -> s { ctm_scale_x = x1*x, ctm_scale_y = y1*y })
                   <*> ctm_scale_x <*> ctm_scale_y

rotateCTM :: Radian -> CTM u -> CTM u
rotateCTM ang1 = (\s ang -> s { ctm_rotation = circularModulo $ ang1+ang })
                   <*> ctm_rotation

translateCTM :: Num u => u -> u -> CTM u -> CTM u
translateCTM x1 y1 = (\s (P2 x y) -> s { ctm_pos = P2 (x+x1) (y+y1) })
                       <*> ctm_pos


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

-- TODO - am I going to consider multi-line labels / textM etc. ?

newtype ShapeLabel = ShapeLabel { getShapeLabel :: String }
  deriving (Eq,Show)



-- Note - labels are not scaled ....
--
labelGraphic :: (Real u, Floating u, FromPtSize u) 
             => ShapeLabel -> CTM u -> Graphic u
labelGraphic sl ctm = 
    monoVecToCenter text >>= \v -> 
    let (P2 x y) = vecdisplace v ctr ; (P2 dx dy) = ctm_pos ctm in
    rotTextline dx dy ang text (ctmDisplace (-x) (-y) ctm)
  where
    (ang,ctr)         = (ctm_rotation ctm, ctm_pos ctm)        
    text              = getShapeLabel sl

-- Labels can only be rotated from their baseline_left, so as we know
-- the center from the CTM we have to find the baseline_left...



rotTextline :: (Real u, Floating u) => u -> u -> Radian -> String -> LocGraphic u
rotTextline dx dy theta ss baseline_left = 
    withTextAttr $ \rgb attr -> 
        singleH $ translatePrim dx dy $ rotatePrim theta 
                                      $ textlabel rgb attr ss baseline_left
     


-- Can all shapes (except coordinates) be stroked and filled?
-- Probably better to have separate type classes...


class DrawShape sh where
  drawShape :: (u ~ DUnit sh) => sh -> Image u sh


-- Just stroke the outline...
--
class OutlineShape sh where
  outlineShape :: (u ~ DUnit sh) => sh -> Image u sh


