{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.PointSupply
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- 
-- \*\* WARNING \*\* - function names likely to change.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.PointSupply
  (
    -- * Constants (might move from this module)
    two_pi
  , half_pi
  
  -- * Generate points.
  , polygonPointsV
  , hpoints
  , vpoints
  
  ) where
  

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import Data.List

two_pi :: Radian
two_pi = 2.0 * pi

half_pi :: Radian
half_pi = 0.5 * pi


-- | 'polygonPointsV' : @ num_points * radius * center -> [point] @ 
--
polygonPointsV :: Floating u => Int -> u -> Point2 u -> [Point2 u]
polygonPointsV n radius = sequence vecs
  where
    theta = two_pi / fromIntegral n
    vecs  = unfoldr phi (0,half_pi)
    
    phi (i,ang) | i < n     = Just ((.+^ avec ang radius), (i+1,ang+theta))
                | otherwise = Nothing


   
-- | 'hpoints' : @ ypos * step * (x0,x1) -> [point] @
-- 
-- Generate points in a horizontal line between x0 and x1.
--
-- Note - the step increment is w.r.t. 0 rather than x0. x0 and 
-- x1 are just the range. An example:
-- 
-- > hpoints 0 10 (5,35)
--
-- > [P2 10 0, P2 20 0, P2 30 0]
--
hpoints :: RealFrac u => u -> u -> (u,u) -> [Point2 u]
hpoints y step (x0,x1) = unfoldr phi start
  where
    start              = initial step x0
    phi st | st < x1   = Just (P2 st y, st+step)  
           | otherwise = Nothing


-- | 'vpoints' : @ xpos * step * (y0,y1) -> [point] @
-- 
-- Generate points in a vertical line between y0 and y1.
--
-- Note - the step increment is w.r.t. 0 rather than y0. y0 and 
-- y1 are just the range. An example:
-- 
-- > vpoints 5 100 (50,500)
--
-- > [P2 5 100, P2 5 200, P2 5 300, P2 5 400]
--
vpoints :: RealFrac u => u -> u -> (u,u) -> [Point2 u]
vpoints x step (y0,y1) = unfoldr phi start
  where
    start              = initial step y0
    phi st | st < y1   = Just (P2 x st, st+step)  
           | otherwise = Nothing



initial :: RealFrac a => a -> a -> a 
initial step minval = step * (fn $ minval / step)
  where
    fn x | x < 0     = fromIntegral $ ceilingi x
         | otherwise = fromIntegral $ 1 + floori x



ceilingi  :: RealFrac a => a -> Integer
ceilingi  = ceiling

floori    :: RealFrac a => a -> Integer
floori    = floor