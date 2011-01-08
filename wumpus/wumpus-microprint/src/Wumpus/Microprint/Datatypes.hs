{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Microprint.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Base datatypes.
--
--------------------------------------------------------------------------------

module Wumpus.Microprint.Datatypes
  (

  -- * Datatypes  
    DrawWordF
  , RenderScalingCtx
  , RenderScalingT
  , makeRenderScalingCtx

  , Tile(..)
  , Height
  , GreekText

  ) where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


-- | 'DrawWordF' :
--
-- > colour * scaled_width * scaled_height -> char_count -> DLocGraphic
--
type DrawWordF = RGBi -> Double -> Double -> Int -> DLocGraphic



type RenderScalingCtx   = ScalingContext Int Int Double
type RenderScalingT m a = ScalingT Int Int Double m a

makeRenderScalingCtx :: (Int -> Double) -> (Int -> Double) -> RenderScalingCtx
makeRenderScalingCtx fx fy = 
    ScalingContext { scale_in_x = fx, scale_in_y = fy }



data Tile = Space Int | Word RGBi Int
  deriving (Eq,Ord,Show)

type Height = Int


-- Note probably better if used a list of lines instead
--
-- > [[Title]] 
--
-- and did not have line break in the Tile datatype.
--
type GreekText = (Height,[[Tile]])


 

