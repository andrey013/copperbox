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

  , horizontal
  , vertical

  , horizontals
  , verticals

  , rescale

  ) where

import Wumpus.Basic.Chains.Base




univariateX :: (Fractional uy, Num ux, Num u) 
            => [ux] -> LocChain ux uy u
univariateX zs = chainFrom $ bivariate (0,zs) gstep
  where
    gstep (_,[])     = Done 
    gstep (n,x:xs)   = Step (x,n) (n+i,xs)
    len              = length zs
    i                = rescale 0 1 0 (fromIntegral $ len-1) 1


univariateY :: (Fractional ux, Num uy, Num u) 
            => [uy] -> LocChain ux uy u
univariateY zs = chainFrom $ bivariate (0,zs) gstep
  where
    gstep (_,[])     = Done 
    gstep (n,y:ys)   = Step (n,y) (n+i,ys)
    len              = length zs
    i                = rescale 0 1 0 (fromIntegral $ len-1) 1



--------------------------------------------------------------------------------
-- Tables


tableDown :: Int -> Int -> Chain Int Int u
tableDown rows cols = 
    chain $ bounded (rows*cols) (iteration (downstep rows) (0,rows-1))



downstep :: Int -> (Int,Int) -> (Int,Int)
downstep row_count (x,y) | y == 0 = (x+1,row_count-1)
downstep _         (x,y)          = (x,y-1)



tableRight :: Num u => Int -> Int -> Chain Int Int u
tableRight rows cols = 
    chain $ bounded (rows*cols) (iteration (rightstep cols) (0,rows-1))


rightstep :: Int -> (Int,Int) -> (Int,Int)
rightstep col_count (x,y) | x == (col_count-1) = (0,y-1)
rightstep _         (x,y)                      = (x+1,y)


horizontal :: Int -> Chain Int Int u
horizontal count = chain $ bivariate 0 alg
  where
    alg st | st == count = Done
    alg st               = Step (st,0) (st+1)


vertical :: Int -> Chain Int Int u
vertical count = chain $ bivariate 0 alg
  where
    alg st | st == count = Done
    alg st               = Step (0,st) (st+1)



horizontals :: (Num ua, Num u) => [ua] -> LocChain ua ua u
horizontals xs0 = chainFrom $ bivariate xs0 alg
  where
    alg []     = Done
    alg (x:xs) = Step (x,0) xs


verticals :: (Num ua, Num u)  => [ua] -> LocChain ua ua u
verticals ys0 = chainFrom $ bivariate ys0 alg
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

