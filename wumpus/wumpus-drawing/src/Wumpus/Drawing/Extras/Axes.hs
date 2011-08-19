{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Extras.Axes
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Drawing grids
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Extras.Axes
  ( 
   
    orthontAxes
 
  , horizontalLabels
  , verticalLabels

  ) where

import Wumpus.Drawing.Connectors

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid

-- Note - axes need labels working out...


-- | Simple orthonormal axes using snap grid units.
--
orthontAxes :: (Real u, Floating u, InterpretUnit u)
            => (Int,Int) -> (Int,Int) -> LocGraphic u
orthontAxes (xl,xr) (yl,yr) = promoteLoc $ \(P2 x y) -> 
    snapmove (1,1) >>= \(V2 uw uh) ->
    let conn1 = rightArrow barb45 connline
        xPtl  = P2 (x - (uw * fromIntegral xl)) y
        xPtr  = P2 (x + (uw * fromIntegral xr)) y
        yPtl  = P2 x (y - (uh * fromIntegral yl))
        yPtr  = P2 x (y + (uh * fromIntegral yr))
    in  localize cap_square $           ignoreAns (connect xPtl xPtr conn1) 
                              `mappend` ignoreAns (connect yPtl yPtr conn1)



horizontalLabels :: (Num a, Fractional u, InterpretUnit u) 
                 => RectAddress -> [a] -> LocGraphic u 
horizontalLabels addr ns = 
    snapmove (1,1) >>= \(V2 uw _) -> runChain_ (mapM mf ns) (chainH uw)
  where
    mf n = onChain $ runPosObject (posTextUpright $ show n) addr


verticalLabels :: (Num a, Fractional u, InterpretUnit u) 
                 => RectAddress -> [a] -> LocGraphic u 
verticalLabels addr ns = 
    snapmove (1,1) >>= \(V2 _ uh) -> runChain_ (mapM mf ns) (chainV uh)
  where
    mf n = onChain $ runPosObject (posTextUpright $ show n) addr
