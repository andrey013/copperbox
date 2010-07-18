{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.PointSupply
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.PointSupply
  (
    hpoints
  , vpoints
  
  ) where
  

import Wumpus.Core                      -- package: wumpus-core

import Data.List


   

hpoints :: RealFrac u => u -> u -> (u,u) -> [Point2 u]
hpoints y step (x0,x1) = unfoldr phi start
  where
    start              = initial step x0
    phi st | st < x1   = Just (P2 st y, st+step)  
           | otherwise = Nothing

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
         | otherwise = fromIntegral $ 1 + floori   x



ceilingi  :: RealFrac a => a -> Integer
ceilingi  = ceiling

floori    :: RealFrac a => a -> Integer
floori    = floor