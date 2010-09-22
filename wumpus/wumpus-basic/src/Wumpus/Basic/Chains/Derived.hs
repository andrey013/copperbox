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
    
    chainFrom

  , univariateX
  , univariateY

  , tableDown
  , tableDownB
  , tableRight

  , horizontal
  , vertical

  , horizontals
  , verticals

  , rescale

  ) where

import Wumpus.Basic.Chains.Base

import Wumpus.Core                              -- package: wumpus-core


chainFrom :: Num u
          => Point2 u -> (ux -> u) -> (uy -> u) -> BivariateAlg ux uy -> Chain u
chainFrom (P2 x0 y0) fx fy alg = 
    chain (\x -> x0 + fx x) (\y -> y0 + fy y) alg

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


tableDownB :: Int -> Int -> BivariateAlg Int Int
tableDownB rows cols = 
    bounded (rows*cols) (iteration (downstep rows) (0,rows-1))

-- | num_rows * num_cols * unit_width * unit_height
tableDown :: Num u => Int -> Int -> u -> u -> Chain u
tableDown rows cols unit_width unit_height = 
    chain (\x -> unit_width  * fromIntegral x) 
          (\y -> unit_height * fromIntegral y)
          (tableDownB rows cols)


downstep :: Int -> (Int,Int) -> (Int,Int)
downstep row_count (x,y) | y == 0 = (x+1,row_count-1)
downstep _         (x,y)          = (x,y-1)



tableRight :: Num u => Int -> Int -> u -> u -> Chain u
tableRight rows cols unit_width unit_height =
    chain (\x -> unit_width  * fromIntegral x) 
          (\y -> unit_height * fromIntegral y)
          alg
  where
    alg = bounded (rows*cols) (iteration (rightstep cols) (0,rows-1))


rightstep :: Int -> (Int,Int) -> (Int,Int)
rightstep col_count (x,y) | x == (col_count-1) = (0,y-1)
rightstep _         (x,y)                      = (x+1,y)


horizontal :: Int -> BivariateAlg Int Int
horizontal count = bivariate 0 alg
  where
    alg st | st == count = Done
    alg st               = Step (st,0) (st+1)


vertical :: Int -> BivariateAlg Int Int
vertical count = bivariate 0 alg
  where
    alg st | st == count = Done
    alg st               = Step (0,st) (st+1)



horizontals :: Num u => [u] -> BivariateAlg u u
horizontals xs0 = bivariate xs0 alg
  where
    alg []     = Done
    alg (x:xs) = Step (x,0) xs


verticals :: Num u => [u] -> BivariateAlg u u
verticals ys0 = bivariate ys0 alg
  where
    alg []     = Done
    alg (y:ys) = Step (0,y) ys


--------------------------------------------------------------------------------
-- general helpers

rescale :: Fractional a => a -> a -> a -> a -> a -> a
rescale outmin outmax innmin innmax a = 
    outmin + innpos * (outrange / innrange)  
  where
    outrange = outmax - outmin
    innrange = innmax - innmin
    innpos   = a - innmin 

