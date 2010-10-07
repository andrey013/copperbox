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
  , ShapeConstructor

  , borderedShape
  , filledShape
  , strokedShape

  -- * ShapeCTM 
  , ShapeCTM(..)
  , identityCTM

  , ctmDisplace
  , ctmCenter


  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative


-- | Note - this formulation prevents rounded corner shapes...
--
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

data Shape u t =  Shape 
      { src_ctm   :: ShapeCTM u 
      , path_fun  :: ShapeCTM u -> Path u
      , cons_fun   :: ShapeCTM u -> t u
      }

type instance DUnit (Shape u sh) = u


type ShapeConstructor u t = ShapeCTM u -> t u 



borderedShape :: Num u => Shape u t -> Image u (t u)
borderedShape (Shape { src_ctm = ctm, path_fun = pf, cons_fun = objf }) = 
   intoImage (pure $ objf ctm) (borderedPath $ toPrimPathU $ pf ctm)

filledShape :: Num u => Shape u t -> Image u (t u)
filledShape (Shape { src_ctm = ctm, path_fun = pf, cons_fun = objf }) = 
   intoImage (pure $ objf ctm) (filledPath $ toPrimPathU $ pf ctm)

strokedShape :: Num u => Shape u t -> Image u (t u)
strokedShape (Shape { src_ctm = ctm, path_fun = pf, cons_fun = objf }) = 
   intoImage (pure $ objf ctm) (closedStroke $ toPrimPathU $ pf ctm)


instance (Real u, Floating u) => Rotate (Shape u sh) where
  rotate r = updateCTM (rotateCTM r)

instance Num u => Scale (Shape u sh) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Shape u sh) where
  translate x y = updateCTM (translateCTM x y)

updateCTM :: (ShapeCTM u -> ShapeCTM u) -> Shape u sh -> Shape u sh
updateCTM fn (Shape ctm pf mkf) = Shape (fn ctm) pf mkf

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
  deriving (Eq,Ord,Show)

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


