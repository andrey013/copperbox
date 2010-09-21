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

  ) where

import Wumpus.Basic.Chains.Base

-- import Wumpus.Core                              -- package: wumpus-core


-- Note - should this be a type representing:
--
-- > ScaleX -> ScaleY -> Chain
--
-- After all, the use-case in mind always constructs it as a
-- partial function.
-- 
univariateX :: (Fractional uy, Num ux) 
            => [ux] -> (ux -> u) -> (uy -> u) -> Chain ux uy u
univariateX zs fX fY = 
    Chain { proj_x      = fX
          , proj_y      = fY
          , st_zero     = (0,zs)
          , gen_step    = gstep  }
  where
    gstep (_,[])     = Done 
    gstep (n,x:xs)   = Step (x,n) (n+i,xs)
    len              = length zs
    i                = rescale 0 1 0 (fromIntegral $ len-1) 1


univariateY :: (Fractional ux, Num uy) 
            => [uy] -> (ux -> u) -> (uy -> u) -> Chain ux uy u
univariateY zs fX fY = 
    Chain { proj_x      = fX
          , proj_y      = fY
          , st_zero     = (0,zs)
          , gen_step    = gstep  }
  where
    gstep (_,[])     = Done 
    gstep (n,y:ys)   = Step (n,y) (n+i,ys)
    len              = length zs
    i                = rescale 0 1 0 (fromIntegral $ len-1) 1



--------------------------------------------------------------------------------
-- Tables


tableDown :: Num u => Int -> Int -> Chain Int Int u 
tableDown rows cols = 
    bounded (rows*cols) (iteration (downstep rows) (0,rows-1)) fromIntegral fromIntegral


downstep :: Int -> (Int,Int) -> (Int,Int)
downstep row_count (x,y) | y == 0 = (x+1,row_count-1)
downstep _         (x,y)          = (x,y-1)



tableRight :: Num u => Int -> Int -> Chain Int Int u 
tableRight rows cols = 
    bounded (rows*cols) (iteration (rightstep cols) (0,rows-1)) fromIntegral fromIntegral


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

