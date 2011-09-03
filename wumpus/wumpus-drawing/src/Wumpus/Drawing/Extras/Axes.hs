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
import qualified Wumpus.Drawing.Connectors.ConnectorPaths as C

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
    snapmove (1,1) >>= \(V2 uw _) -> runChain_ (chainH uw) (mapM mf ns) 
  where
    mf n = onChain $ runPosObject addr $ posTextUpright $ show n


verticalLabels :: (Num a, Fractional u, InterpretUnit u) 
               => RectAddress -> [a] -> LocGraphic u 
verticalLabels addr ns = 
    snapmove (1,1) >>= \(V2 _ uh) -> runChain_ (chainV uh) (mapM mf ns)
  where
    mf n = onChain $ runPosObject addr $ posTextUpright $ show n



-- Cf. Parsec\'s Token module...
--
connline :: (Real u, Floating u, InterpretUnit u) => ConnectorPathQuery u
connline = C.connline default_connector_props
