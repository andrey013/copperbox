{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Chains.Derived
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Generate points in an iterated chain.
--
-- WARNING - very unstable.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Chains.Derived
  (

    univariateX
  , univariateY

  , tableDown
  , tableRight

  , rescale

  ) where

import Wumpus.Basic.Chains.Base

-- import Wumpus.Core                              -- package: wumpus-core


univariateX :: (Fractional uy, Num ux) 
            => [ux] -> BivariateAlg ux uy
univariateX zs = bivariate (0,zs) gstep
  where
    gstep (_,[])     = Done 
    gstep (n,x:xs)   = Step (x,n) (n+i,xs)
    len              = length zs
    i                = rescale 0 1 0 (fromIntegral $ len-1) 1


univariateY :: (Fractional ux, Num uy) 
            => [uy] -> BivariateAlg ux uy
univariateY zs = bivariate (0,zs) gstep
  where
    gstep (_,[])     = Done 
    gstep (n,y:ys)   = Step (n,y) (n+i,ys)
    len              = length zs
    i                = rescale 0 1 0 (fromIntegral $ len-1) 1



--------------------------------------------------------------------------------
-- Tables


tableDown :: Num u => Int -> Int -> u -> u -> Chain u
tableDown rows cols row_height col_width = 
    chain (\x -> col_width  * fromIntegral x) 
          (\y -> row_height * fromIntegral y)
          alg
  where
    alg = bounded (rows*cols) (iteration (downstep rows) (0,rows-1))


downstep :: Int -> (Int,Int) -> (Int,Int)
downstep row_count (x,y) | y == 0 = (x+1,row_count-1)
downstep _         (x,y)          = (x,y-1)



tableRight :: Num u => Int -> Int -> u -> u -> Chain u
tableRight rows cols row_height col_width =
    chain (\x -> col_width  * fromIntegral x) 
          (\y -> row_height * fromIntegral y)
          alg
  where
    alg = bounded (rows*cols) (iteration (rightstep cols) (0,rows-1))


rightstep :: Int -> (Int,Int) -> (Int,Int)
rightstep col_count (x,y) | x == (col_count-1) = (0,y-1)
rightstep _         (x,y)                      = (x+1,y)



--------------------------------------------------------------------------------
-- general helpers

rescale :: Fractional a => a -> a -> a -> a -> a -> a
rescale outmin outmax innmin innmax a = 
    outmin + innpos * (outrange / innrange)  
  where
    outrange = outmax - outmin
    innrange = innmax - innmin
    innpos   = a - innmin 

