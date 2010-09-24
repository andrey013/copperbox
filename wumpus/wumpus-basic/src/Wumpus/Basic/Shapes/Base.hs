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
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes.Base
  ( 


    Shape(..)

  , drawShape

  -- * ShapeCTM 
  , ShapeCTM(..)
  , identityCTM

  , ctmDisplace
  , ctmCenter

  ) where

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic


import Control.Applicative



data Shape u sh =  Shape 
      { src_ctm :: ShapeCTM u 
      , out_fun :: ShapeCTM u -> Image u sh
      }

type instance DUnit (Shape u sh) = u

drawShape :: Shape u sh -> Image u sh
drawShape (Shape { src_ctm = ctm, out_fun = fn }) = fn ctm 


instance (Real u, Floating u) => Rotate (Shape u sh) where
  rotate r = updateCTM (rotateCTM r)

instance Num u => Scale (Shape u sh) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Shape u sh) where
  translate x y = updateCTM (translateCTM x y)

updateCTM :: (ShapeCTM u -> ShapeCTM u) -> Shape u sh -> Shape u sh
updateCTM fn (Shape ctm out) = Shape (fn ctm) out

--------------------------------------------------------------------------------
-- CTM

-- Note - all shapes need a location (usually/always the center)
-- so this needs to be stored in the CTM.
--

data ShapeCTM u = ShapeCTM 
      { ctm_trans_x             :: !u
      , ctm_trans_y             :: !u
      , ctm_scale_x             :: !u
      , ctm_scale_y             :: !u
      , ctm_rotation            :: Radian
      }
  deriving (Eq,Show)

type instance DUnit (ShapeCTM u) = u

identityCTM :: Num u => ShapeCTM u
identityCTM = ShapeCTM { ctm_trans_x  = 0 
                       , ctm_trans_y  = 0 
                       , ctm_scale_x  = 1
                       , ctm_scale_y  = 1
                       , ctm_rotation = 0 }





scaleCTM :: Num u => u -> u -> ShapeCTM u -> ShapeCTM u
scaleCTM sx sy = 
    (\s x y -> s { ctm_scale_x = x*sx, ctm_scale_y = y*sy })
      <*> ctm_scale_x <*> ctm_scale_y


rotateCTM :: Radian -> ShapeCTM u -> ShapeCTM u
rotateCTM ang1 = 
    (\s ang -> s { ctm_rotation = circularModulo $ ang1+ang })
      <*> ctm_rotation


translateCTM :: Num u => u -> u -> ShapeCTM u -> ShapeCTM u
translateCTM dx dy = 
    (\s x y -> s { ctm_trans_x = x+dx, ctm_trans_y = y+dy })
      <*> ctm_trans_x <*> ctm_trans_y


ctmDisplace :: (Real u, Floating u) => Point2 u -> ShapeCTM u -> Point2 u
ctmDisplace (P2 x y) (ShapeCTM { ctm_trans_x  = dx, ctm_trans_y  = dy 
                               , ctm_scale_x  = sx, ctm_scale_y  = sy
                               , ctm_rotation = theta }) = 
    translate dx dy $ rotate theta $ P2 (sx*x) (sy*y)


ctmCenter :: (Real u, Floating u) => ShapeCTM u -> Point2 u
ctmCenter = ctmDisplace zeroPt

