{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Microprint.Render
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Render
--
--------------------------------------------------------------------------------

module Wumpus.Microprint.Render
  (
    RenderScalingCtx
  , makeRenderScaling
  , DrawWordF  
  , greekF
  , strokelineF
  , borderedF

  , render

  ) where


import Wumpus.Microprint.Datatypes 

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Data.AffineSpace				-- package: vector-space


--------------------------------------------------------------------------------

type RenderScalingCtx = ScalingContext Int Int Double
type RenderScalingT m a = ScalingT Int Int Double m a

makeRenderScaling :: (Int -> Double) -> (Int -> Double) -> ScalingContext Int Int Double
makeRenderScaling fx fy = 
    ScalingContext { scale_in_x = fx, scale_in_y = fy }


-- | 'DrawWordF' :
--
-- > colour * scaled_width * scaled_height -> char_count -> DLocGraphic
--
type DrawWordF = RGBi -> Double -> Double -> Int -> DLocGraphic


-- | Just a filled rectangle.
--
greekF :: DrawWordF
greekF rgb w h _ = localize (fillColour rgb) (filledRectangle w h)



-- This needs re-working....
borderedF :: DrawWordF
borderedF rgb w h i = ticks background
  where
    background  = localize (fillColour rgb) (borderedRectangle w h)
    v1          = hvec $ w / fromIntegral i
    ticks g1      = promote1 (\pt -> oconcat (g1 `at` pt) $ 
                                  map (straightLine (vvec h) `at`) 
    	                            $ take (i-1) $ iterate (.+^ v1) (pt .+^ v1) )

 
-- | A stroked line.
--
strokelineF :: DrawWordF
strokelineF rgb w _ _ = localize (strokeColour rgb) (straightLine (hvec w))


render :: RenderScalingCtx -> DrawWordF -> GreekText -> TraceDrawing Double ()
render ctx wordDraw (hmax,xs) = 
    runScalingT ctx $ mstep hmax xs
  where
    mstep h (s:ss) = renderLine wordDraw h s >> mstep (h-1) ss 
    mstep _ _      = return ()

renderLine :: DrawWordF -> Int -> [Tile] 
	   -> RenderScalingT (TraceDrawing Double) ()
renderLine fn h ts = mstep 0 ts
  where
    mstep x (Word rgb n:xs) = draw1 fn rgb n (x,h) >> mstep (x+n) xs
    mstep x (Space n:xs)    = mstep (x+n) xs  
    mstep _ []              = return ()

draw1 :: DrawWordF -> RGBi -> Int -> (Int,Int) 
      -> RenderScalingT (TraceDrawing Double) () 
draw1 fn rgb n (x,y)  = 
    scalePt x y >>= \pt -> scaleX n >>= \w -> unitY >>= \h ->  
    draw $ fn rgb w h n `at` pt

