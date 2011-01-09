{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.ScalingContext
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Scaling in X and Y
--
-- \*\* WARNING \*\* - half baked.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.ScalingContext
  (

    ScalingContext(..)

  , scaleX
  , scaleY
  , scalePt
  , scaleVec

  , unitX
  , unitY

  , uniformScaling
  , coordinateScaling

  ) where


import Wumpus.Core				-- package: wumpus-core



-- | ScalingContext is a dictionary of two functions for scaling 
-- in X and Y.
--
data ScalingContext ux uy u = ScalingContext
      { scale_in_x  :: ux -> u
      , scale_in_y  :: uy -> u
      }


scaleX              :: ScalingContext ux uy u -> ux -> u
scaleX ctx ux       = (scale_in_x ctx) ux

scaleY              :: ScalingContext ux uy u -> uy -> u
scaleY ctx uy       = (scale_in_y ctx) uy


scalePt             :: ScalingContext ux uy u -> ux -> uy -> Point2 u
scalePt ctx ux uy   = P2 (scale_in_x ctx ux) (scale_in_y ctx uy)

scaleVec            :: ScalingContext ux uy u -> ux -> uy -> Vec2 u
scaleVec ctx ux uy  = V2 (scale_in_x ctx ux) (scale_in_y ctx uy)


unitX               :: Num ux => ScalingContext ux uy u -> u
unitX ctx           = scaleX ctx 1
 
unitY               :: Num uy => ScalingContext ux uy u -> u
unitY ctx           = scaleY ctx 1




--------------------------------------------------------------------------------
-- constructors for scaling context


-- | Build a ScalingContext where both X and Y are scaled by the 
-- same uniform step.
--
-- The dimensions (types) of the ScalingContext are unified - the 
-- output type and the input types are all the same.
--
uniformScaling :: Num u => u -> ScalingContext u u u
uniformScaling u = ScalingContext
      { scale_in_x  = (\x -> u*x)
      , scale_in_y  = (\y -> u*y)
      }



-- | Build a ScalingContext for scaling Int coordinates.
--
-- The scaling factors in X and Y can be different sizes.
---
coordinateScaling :: Num u => u -> u -> ScalingContext Int Int u
coordinateScaling sx sy = ScalingContext
      { scale_in_x  = (\x -> sx * fromIntegral x)
      , scale_in_y  = (\y -> sy * fromIntegral y)
      }



