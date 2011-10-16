{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Vamps
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Library of vamps (currently small).
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Vamps
  ( 

    squareWE

  ) where

import Wumpus.Drawing.Paths.PathBuilder ( Vamp(..) )
import Wumpus.Drawing.Paths.Base

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core


-- TODO - library of useful / illustrative vamps (circle, square etc.)



-- 
--
squareWE :: (Real u, Floating  u, Ord u, Tolerance u, InterpretUnit u) 
         => u -> Vamp u
squareWE diam = Vamp { vamp_move = hvec diam
                     , vamp_conn = conn }
  where
    conn = promoteConn $ \p1 p2 -> 
             let dir = vdirection $ pvec p1 p2
             in renderPath_ CSTROKE $ vectorPathTheta path1 dir p1

    hdiam = 0.5 * diam 
    path1 = [ vvec hdiam, hvec diam, vvec (-diam), hvec (-diam) ]


-- Drawing a cirle picks the half point on the vamp_move vector...

