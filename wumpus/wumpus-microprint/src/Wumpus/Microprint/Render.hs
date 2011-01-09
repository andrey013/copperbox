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

    greekF
  , strokelineF
  , borderedF

  , render

  ) where


import Wumpus.Microprint.Datatypes 

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Data.AffineSpace				-- package: vector-space


--------------------------------------------------------------------------------





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
    ticks g1    = promoteR1 (\pt -> oconcat (g1 `at` pt) $ 
                                  map (straightLine (vvec h) `at`) 
    	                            $ take (i-1) $ iterate (.+^ v1) (pt .+^ v1) )

 
-- | A stroked line.
--
strokelineF :: DrawWordF
strokelineF rgb w _ _ = localize (strokeColour rgb) (straightLine (hvec w))


render :: RenderScalingCtx -> DrawWordF -> GreekText -> TraceDrawing Double ()
render ctx wordDraw (hmax,xs) = mstep hmax xs
  where
    mstep h (s:ss) = renderLine ctx wordDraw h s >> mstep (h-1) ss 
    mstep _ _      = return ()

renderLine :: RenderScalingCtx -> DrawWordF -> Int -> [Tile] 
	   -> TraceDrawing Double ()
renderLine ctx fn h ts = mstep 0 ts
  where
    mstep x (Word rgb n:xs) = draw1 ctx fn rgb n (x,h) >> mstep (x+n) xs
    mstep x (Space n:xs)    = mstep (x+n) xs  
    mstep _ []              = return ()

draw1 :: RenderScalingCtx -> DrawWordF -> RGBi -> Int -> (Int,Int) 
      -> TraceDrawing Double ()
draw1 ctx fn rgb n (x,y)  = 
    let pt = scalePt ctx x y 
        w  = scaleX ctx n 
        h  = unitY ctx
    in draw $ fn rgb w h n `at` pt

