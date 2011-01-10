{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Chains.Derived
-- Copyright   :  (c) Stephen Tetley 2010-2011
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

module Wumpus.Drawing.Chains.Derived
  (
    
    tableDown
  , tableRight

  , horizontal
  , vertical

  , horizontals
  , verticals

  ) where

import Wumpus.Drawing.Chains.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

--------------------------------------------------------------------------------
-- Tables

-- Note - for the minor runtime cost, pairing the row_width and 
-- row_height should make the API more /memorable/...


-- | 'tableDown' : @ num_rows * (row_width, row_height) -> LocChain @
--
-- The table grows down and right, the implicit initial point is 
-- @top-left@.
--
tableDown :: Num u => Int -> (u,u) -> LocChain u
tableDown n (rw,rh) pt = map fn ints
  where
    ints = iterate (+1) 0
    fn i = let (x,y) = i `divMod` n 
           in displace (rw * fromIntegral x) (rh * fromIntegral (-y)) pt


-- | 'tableRight' : @ num_cols * row_width * row_height -> LocChain @
--
-- The table grows right and down, the implicit initial point is 
-- @top-left@.
--
-- This chain is infinite.
--
tableRight :: Num u => Int -> (u,u) -> LocChain u
tableRight n (rw,rh) pt = map fn ints
  where
    ints = iterate (+1) 0
    fn i = let (y,x) = i `divMod` n 
           in displace (rw * fromIntegral x) (rh * fromIntegral (-y)) pt





-- | 'horizontal' : @ horizontal_dist -> LocChain @
--
-- The chain grows right by the supplied increment.
--
-- This chain is infinite.
--
-- \*\* WARNING \*\* - name due to be changed. Current name is 
-- too general for this function. 
--
horizontal :: Num u => u -> LocChain u
horizontal dx = iterate (displaceH dx)


-- | 'vertical' : @ vertical_dist -> LocChain @
--
-- The chain grows up by the supplied increment.
--
-- This chain is infinite.
--
-- \*\* WARNING \*\* - name due to be changed. Current name is 
-- too general for this function. 
--
vertical :: Num u => u -> LocChain u
vertical dy = iterate (displaceV dy) 


-- | 'horizontals' : @ [horizontal_dist] -> LocChain @
--
-- This is a @scanl@ successive displacing the start point.
--
-- This chain is finite (for finite input list).
--
-- \*\* WARNING \*\* - name due to be changed. Current name is 
-- too general for this function. 
--
horizontals :: Num u => [u] -> LocChain u
horizontals xs = \pt -> scanl (flip displaceH) pt xs 


-- | 'verticals' : @ [vertical_dist] -> LocChain @
--
-- This is a @scanl@ successive displacing the start point.
--
-- This chain is finite (for finite input list).
--
-- \*\* WARNING \*\* - name due to be changed. Current name is 
-- too general for this function. 
--
verticals :: Num u => [u] -> LocChain u
verticals ys = \pt -> scanl (flip displaceV) pt ys


