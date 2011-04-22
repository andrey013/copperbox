{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Connectors.BoxConnectors
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Box connectors
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Connectors.BoxConnectors
  ( 
    ConnectorBox
  , connbox

  ) where

-- import Wumpus.Drawing.Paths.Absolute

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


-- NOTE - boxes (currently) seem to only support stroke otherwise
-- they would obliterate what they connect.



-- | The type of BoxConnectors - a query from start and end point 
-- to a Path. 
-- 
-- Note - unlike a @Connector@, a BoxConnnector is expected to be 
-- closed, then filled, stroked or bordered.
--
type ConnectorBox u = ConnectorGraphic u


--
-- DESIGN NOTE - boxes (probably) should not use source and dest
-- separators.
--


-- | Draw a stroked, rectangular box around the connector points.
--
-- The rectangle will be inclined to the line.
--
connbox :: (Real u, Floating u, InterpretUnit u) 
        => ConnectorBox u
connbox = promoteR2 $ \p0 p1 -> 
    connectorSrcArm >>= \src_arm ->
    connectorDstArm >>= \dst_arm ->
    let ang = vdirection $ pvec p0 p1 
        bl  = displaceOrtho (V2 (-src_arm) (-src_arm)) ang p0
        tl  = displaceOrtho (V2 (-src_arm)   src_arm ) ang p0
        br  = displaceOrtho (V2   dst_arm  (-src_arm)) ang p1
        tr  = displaceOrtho (V2   dst_arm    src_arm ) ang p1
    in vertexPP [ bl, br, tr, tl ] >>= dcClosedPath STROKE