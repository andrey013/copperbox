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

  ) where

import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Connectors.ConnectorPaths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( black )


-- Note - axes need labels working out...


-- | Draw axes from (0,0).
--
orthontAxes :: (Real u, Floating u, InterpretUnit u)
            => (Double, Double) -> Graphic u
orthontAxes (w,h) = 
    snapmove (1,1) >>= \(V2 w1 h1) ->
    let conn1 = rightArrow barb45 connline
        uw    = w1 * realToFrac w
        uh    = h1 * realToFrac h
        ptX   = displaceH uw zeroPt
        ptY   = displaceV uh zeroPt
    in  localize cap_square $         graphic_ (connect conn1 zeroPt ptX) 
                              `oplus` graphic_ (connect conn1 zeroPt ptY)


    
    
