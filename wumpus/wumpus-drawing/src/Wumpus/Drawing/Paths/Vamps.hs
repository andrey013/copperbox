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

import Wumpus.Drawing.Paths.Base.PathBuilder ( Vamp(..) )
import Wumpus.Drawing.Paths.Base.AbsPath

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core



-- TODO - library of useful / illustrative vamps (circle, square etc.)



-- Note - actual square TODO...
--
squareWE :: (Real u, Floating  u, Ord u, Tolerance u, InterpretUnit u) 
         => u -> Vamp u
squareWE diam = Vamp { vamp_move = hvec diam
                     , vamp_conn = conn }
  where
    conn = promoteConn $ \p1 p2 -> 
           -- TODO ...
           drawClosedPath_ STROKE $ vertexPath $ [ p1, p2 ]

     -- vertexPath [ vvec hdiam, hvec diam, vvec (-diam), hvec (-diam) ]


-- Drawing a cirle picks the half point on the vamp_move vector...
