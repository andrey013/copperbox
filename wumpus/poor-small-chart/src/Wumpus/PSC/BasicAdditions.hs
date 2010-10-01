{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.BasicAdditions
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Candidates for moving to Wumpus-Basic
-- 
--------------------------------------------------------------------------------

module Wumpus.PSC.BasicAdditions
  (

{-  
    IndexAlg
  , ixStart
  , ixStarti
  , textlineRect
  , TextlineRectDisplace
  , frameWest
  , frameNorth
  , pointHylo
  , pointHylo2
-}

  ) where

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Graphic                     -- package: wumpus-basic

{-


-- (min,step)
type IndexAlg ua = (ua,ua)

ixStart :: (Fractional ua, RealFrac ua) => IndexAlg ua -> ua
ixStart (minval, step) = step * ceilingFro (minval / step)
  where
    ceilingFro = fromInteger . ceiling

ixStarti :: (Integral ua) => IndexAlg ua -> ua
ixStarti (minval, step) = step * ceiling (minval `divi` step)

divi :: Integral a => a -> a -> Double 
divi a b = fromIntegral a / fromIntegral b




textlineRect :: (Fractional u, FromPtSize u) 
             => (RGBi,FontAttr) -> String -> (Rectangle u, GraphicF u)
textlineRect (rgb,attr) text  = 
    (Rectangle text_width text_height, wrapG . textlabel (rgb,attr) text)
  where
    pt_size       = font_size attr
    text_height   = fromPtSize $ numeralHeight pt_size
    text_width    = fromPtSize $ textWidth  pt_size (length text)


type TextlineRectDisplace u = (Rectangle u, GraphicF u) -> GraphicF u
    
frameWest :: Fractional u => TextlineRectDisplace u
frameWest (Rectangle w h, gf) = gf . disp (-w) (negate $ 0.5*h)

frameNorth :: Fractional u => TextlineRectDisplace u
frameNorth (Rectangle w h, gf) = gf . disp (negate $ 0.5*w) (-h)



--------------------------------------------------------------------------------



hylor :: (st -> Maybe (a,st)) -> (a -> b -> b) -> b -> st -> b
hylor f g b0 st0 = step b0 (f st0)
  where
    step acc Nothing        = acc
    step acc (Just (a,st))  = step (g a acc) (f st) 


-- Simple type restricted hylomorphism - generates points, 
-- amalgamates to a Graphic.
--
pointHylo :: (st -> Maybe (Point2 u, st)) 
          -> (Point2 u -> Graphic u) 
          -> st 
          -> Graphic u
pointHylo f g = hylor f (\pt acc -> acc . g pt) emptyG


pointHylo2 :: (st -> Maybe ((a,Point2 u), st)) 
           -> (a -> Point2 u -> Graphic u) 
           -> st 
           -> Graphic u
pointHylo2 g f  = hylor g (\(a,pt) acc -> acc . f a pt) emptyG

-}