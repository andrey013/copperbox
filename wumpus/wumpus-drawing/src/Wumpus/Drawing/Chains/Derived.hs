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


  , innerHorizontals
  , innerVerticals


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
tableDown :: Num u => Int -> (u,u) -> [LocImage u zz] -> LocImage u (Point2 u)
tableDown n (rw,rh) = chainIterate succ gen 0
  where
    gen i   = let (x,y) = i `divMod` n 
              in displace (rw * fromIntegral x) (rh * fromIntegral (-y))


-- | 'tableRight' : @ num_cols * row_width * row_height -> LocChain @
--
-- The table grows right and down, the implicit initial point is 
-- @top-left@.
--
tableRight :: Num u => Int -> (u,u) -> [LocImage u zz] -> LocImage u (Point2 u)
tableRight n (rw,rh) = chainIterate succ gen 0
  where
    gen i   = let (y,x) = i `divMod` n 
              in displace (rw * fromIntegral x) (rh * fromIntegral (-y))



-- | 'chainH' : @ horizontal_dist -> LocChain @
--
-- The chain grows right by the supplied increment.
--
chainH :: Num u => u -> [LocImage u zz] -> LocImage u (Point2 u)
chainH dx = chainDisplace (displaceH dx)



-- | 'verticalPoints' : @ vertical_dist -> LocChain @
--
-- The chain grows up by the supplied increment.
--
chainV :: Num u => u -> [LocImage u zz] -> LocImage u (Point2 u)
chainV dy = chainDisplace (displaceV dy)



-- | 'chainRadial' : 
-- @ radius * start_ang * rot_ang * [loc_fun] -> LocImage (end_point) @
--
-- Distribute the list of @loc_fun@ whilst the start point is 
-- iterated with radially the supplied angle.
--
chainRadial :: (Floating u)
                 => u -> Radian -> Radian -> [LocImage u zz] 
                 -> LocImage u (Point2 u)
chainRadial radius start_ang rot_ang = 
    chainIterate (+ rot_ang) (\a -> displaceVec (avec a radius)) start_ang




-- | 'chainStepsH' : @ [horizontal_dist] -> LocChain @
--
-- This is a @scanl@ successive displacing the start point.
--
chainStepsH :: Num u => [u] -> LocImage u zz -> LocImage u (Point2 u) 
chainStepsH xs gf = apChainDisplace id fn (scanl (+) 0 xs)
  where
    fn dx = moveStart (displaceH dx) gf 



-- | 'chainStepsV' : @ [vertical_dist] -> LocChain @
--
-- This is a @scanl@ successive displacing the start point.
--
chainStepsV :: Num u => [u] -> LocImage u zz -> LocImage u (Point2 u) 
chainStepsV xs gf = apChainDisplace id fn (scanl (+) 0 xs)
  where
    fn dx = moveStart (displaceV dx) gf 


{-# INLINE [0] ceilingi #-}
ceilingi :: RealFrac a => a -> Int
ceilingi = ceiling


-- | Note - horizontals are projected from the start point. The 
-- horizontal component of the second point is ignored.
-- 
-- This chain is finite for well formed input.
--
innerHorizontals :: RealFrac u 
                 => u -> LocImage u zz -> ConnectorGraphic u
innerHorizontals w gf = promoteR2 $ \(P2 x0 y0) (P2 x1 _) -> 
    let xc = fromIntegral $ ceilingi $ x0 / w
        n  = floor $  (x1 - x0) / w 
        xs = take n $ repeat w
    in ignoreAns $ chainStepsH xs gf `at` (P2 xc y0) 
      

-- | Note - verticals are projected from the start point. The 
-- vertical component of the second point is ignored.
-- 
-- This chain is finite for well formed input.
--
innerVerticals :: RealFrac u => u -> LocImage u zz -> ConnectorGraphic u
innerVerticals h gf = promoteR2 $ \(P2 x0 y0) (P2 _ y1) -> 
    let yc = fromIntegral $ ceilingi $ y0 / h
        n  = floor $  (y1 - y0) / h
        xs = take n $ repeat h
    in ignoreAns $ chainStepsV xs gf `at` (P2 x0 yc) 
      
