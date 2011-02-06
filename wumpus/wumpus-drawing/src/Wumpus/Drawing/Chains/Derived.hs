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

  , horizontalPoints
  , verticalPoints

  , horizontalSteps
  , verticalSteps

  , innerHorizontals
  , innerVerticals

  ) where

import Wumpus.Drawing.Chains.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


import Data.List 

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
tableDown n (rw,rh) = promoteR1 $ \pt -> return $ map (fn pt) ints
  where
    ints    = iterate (+1) 0
    fn pt i = let (x,y) = i `divMod` n 
              in displace (rw * fromIntegral x) (rh * fromIntegral (-y)) pt


-- | 'tableRight' : @ num_cols * row_width * row_height -> LocChain @
--
-- The table grows right and down, the implicit initial point is 
-- @top-left@.
--
-- This chain is infinite.
--
tableRight :: Num u => Int -> (u,u) -> LocChain u
tableRight n (rw,rh) = promoteR1 $ \pt -> return $ map (fn pt) ints
  where
    ints     = iterate (+1) 0
    fn pt i = let (y,x) = i `divMod` n 
              in displace (rw * fromIntegral x) (rh * fromIntegral (-y)) pt





-- | 'horizontalPoints' : @ horizontal_dist -> LocChain @
--
-- The chain grows right by the supplied increment.
--
-- This chain is infinite.
--
horizontalPoints :: Num u => u -> LocChain u
horizontalPoints dx = promoteR1 $ \pt -> return $ iterate (displaceH dx) pt


-- | 'verticalPoints' : @ vertical_dist -> LocChain @
--
-- The chain grows up by the supplied increment.
--
-- This chain is infinite.
--
verticalPoints :: Num u => u -> LocChain u
verticalPoints dy = promoteR1 $ \pt -> return $ iterate (displaceV dy) pt


-- | 'horizontalSteps' : @ [horizontal_dist] -> LocChain @
--
-- This is a @scanl@ successive displacing the start point.
--
-- This chain is finite (for finite input list).
--
horizontalSteps :: Num u => [u] -> LocChain u
horizontalSteps xs = promoteR1 $ \pt -> return $ scanl (flip displaceH) pt xs 


-- | 'verticalSteps' : @ [vertical_dist] -> LocChain @
--
-- This is a @scanl@ successive displacing the start point.
--
-- This chain is finite (for finite input list).
--
-- \*\* WARNING \*\* - name due to be changed. Current name is 
-- too general for this function. 
--
verticalSteps :: Num u => [u] -> LocChain u
verticalSteps ys = promoteR1 $ \pt -> return $ scanl (flip displaceV) pt ys


{-# INLINE [0] ceilingi #-}
ceilingi :: RealFrac a => a -> Int
ceilingi = ceiling


-- | Note - horizontals are projected from the start point. The 
-- horizontal component of the second point is ignored.
-- 
-- This chain is finite for well formed input.
--
innerHorizontals :: RealFrac u => u -> ConnectorChain u
innerHorizontals n = promoteR2 $ \a b -> return $ body a b
  where
    body (P2 x0 y0) (P2 x1 _) = unfoldr phi (n * fromIntegral z) 
      where z                 = ceilingi $ x0 / n
            phi x | x < x1    = Just (P2 x y0, x+n)
                  | otherwise = Nothing

      

-- | Note - verticals are projected from the start point. The 
-- vertical component of the second point is ignored.
-- 
-- This chain is finite for well formed input.
--
innerVerticals :: RealFrac u => u -> ConnectorChain u
innerVerticals n = promoteR2 $ \a b -> return $ body a b
  where
    body (P2 x0 y0) (P2 _ y1) = unfoldr phi (n * fromIntegral z)
      where z                 = ceilingi $ y0 / n
            phi y | y < y1    = Just (P2 x0 y, y+n)
                  | otherwise = Nothing

