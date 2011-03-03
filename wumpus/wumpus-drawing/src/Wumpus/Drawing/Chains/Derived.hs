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

  , chainH
  , chainV
  , chainRadial
  , chainStepsH
  , chainStepsV

  ) where

import Wumpus.Drawing.Chains.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


--------------------------------------------------------------------------------
-- Tables

-- Note - for the minor runtime cost, pairing the row_width and 
-- row_height should make the API more /memorable/...



-- | 'tableDown' : @ num_rows * (row_width, row_height) -> LocChain @
--
-- The table grows down and right, the implicit initial point is 
-- @top-left@.
--
tableDown :: PtSize u => Int -> (u,u) -> [LocImage t u] -> LocImage Point2 u
tableDown n (rw,rh) = chainIterate succ gen 0
  where
    gen i   = let (x,y) = i `divMod` n 
              in displace (rw * fromIntegral x) (rh * fromIntegral (-y))


-- | 'tableRight' : @ num_cols * row_width * row_height -> LocChain @
--
-- The table grows right and down, the implicit initial point is 
-- @top-left@.
--
tableRight :: PtSize u => Int -> (u,u) -> [LocImage t u] -> LocImage Point2 u
tableRight n (rw,rh) = chainIterate succ gen 0
  where
    gen i   = let (y,x) = i `divMod` n 
              in displace (rw * fromIntegral x) (rh * fromIntegral (-y))



-- | 'chainH' : @ horizontal_dist -> LocChain @
--
-- The chain grows right by the supplied increment.
--
chainH :: PtSize u => u -> [LocImage t u] -> LocImage Point2 u
chainH dx = chainDisplace (displaceH dx)



-- | 'verticalPoints' : @ vertical_dist -> LocChain @
--
-- The chain grows up by the supplied increment.
--
chainV :: PtSize u => u -> [LocImage t u] -> LocImage Point2 u
chainV dy = chainDisplace (displaceV dy)



-- | 'chainRadial' : 
-- @ radius * start_ang * rot_ang * [loc_fun] -> LocImage (end_point) @
--
-- Distribute the list of @loc_fun@ whilst the start point is 
-- iterated with radially the supplied angle.
--
chainRadial :: (Floating u, PtSize u)
            => u -> Radian -> Radian -> [LocImage t u] -> LocImage Point2 u
chainRadial radius start_ang rot_ang = 
    chainIterate (+ rot_ang) (\a -> displaceVec (avec a radius)) start_ang




-- | 'chainStepsH' : @ [horizontal_dist] -> LocChain @
--
-- This is a @scanl@ successive displacing the start point.
--
chainStepsH :: PtSize u => [u] -> [LocImage t u] -> LocImage Point2 u
chainStepsH xs gs = interChainDisplace (map displaceH xs) gs




-- | 'chainStepsV' : @ [vertical_dist] -> LocChain @
--
-- This is a @scanl@ successive displacing the start point.
--
chainStepsV :: PtSize u => [u] -> [LocImage t u] -> LocImage Point2 u
chainStepsV xs gs = interChainDisplace (map displaceV xs) gs
 


