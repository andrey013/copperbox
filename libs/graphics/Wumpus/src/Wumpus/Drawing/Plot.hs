{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Plot
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Plotting
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.Plot where

import Wumpus.Core.Curve
import Wumpus.Core.Point


hyperbola :: Double -> Double -> Int -> [DCurve]
hyperbola x0 x1 n = cs
  where
    (y0,s) = f2 x0
    h      = (x1-x0)/ fromIntegral  n
    cs     = step n (x0,y0,s) []
    step 0 _   acc = reverse acc 
    step i alg acc = let (alg',a) = makeCurve alg h in step (i-1) alg' (a:acc)

    
f2 :: Floating a => a -> (a,a)
f2 x = (y,x/y) where y = sqrt (1+pow2 x)

pow2 :: Floating a => a -> a 
pow2 = flip (^^) two
  where 
    two :: Integer
    two = 2

type Alg = (Double,Double,Double)


makeCurve :: Alg -> Double -> (Alg,DCurve)
makeCurve (x0,y0,s) h = 
    ((x3,y3,s'), Curve (P2 x0 y0) (P2 x1 y1) (P2 x2 y2)  (P2 x3 y3) )
  where
    x1      = x0 + h/3
    y1      = y0 + s * h/3
    x3      = x0 + h
    (y3,s') = f2 x3
    x2      = x3 - h/3
    y2      = y3 - s'*(h/3)


--------------------------------------------------------------------------------
