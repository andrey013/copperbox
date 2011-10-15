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
  , ConnectorBoxSpec(..)
  , renderConnectorBoxSpec

  , conn_box
  , conn_tube
  , conn_chamf_box 


  ) where

import Wumpus.Drawing.Basis.InclineTrails
import Wumpus.Drawing.Connectors.ConnectorProps

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space


-- NOTE - boxes seem to only support stroke otherwise
-- they would obliterate what they connect.



-- | The type of BoxConnectors - a query from start and end point 
-- to a Path. 
-- 
-- Note - unlike a @Connector@, a BoxConnnector is expected to be 
-- closed, then filled, stroked or bordered.
--
type ConnectorBox u = ConnectorGraphic u

newtype ConnectorBoxSpec u = ConnectorBoxSpec { 
      getConnectorBoxSpec :: ConnectorProps -> ConnectorBox u }


renderConnectorBoxSpec :: (Real u, Floating u, InterpretUnit u)
                       => ConnectorProps
                       -> ConnectorBoxSpec u
                       -> ConnectorBox u
renderConnectorBoxSpec props spec = 
    getConnectorBoxSpec spec props





--
-- DESIGN NOTE - boxes (probably) should not use source and dest
-- separators.
--

boxConnector :: (Floating u, Ord u, InterpretUnit u) 
             => (ConnectorProps -> Point2 u -> Point2 u -> Image u a) 
             -> ConnectorBoxSpec u
boxConnector mf = ConnectorBoxSpec $ \props -> 
    promoteConn $ \p0 p1 -> ignoreAns $ mf props p0 p1 


adaptAnaTrail :: (Real u, Floating u, Ord u, InterpretUnit u) 
              => (u -> Vec2 u -> AnaTrail u) 
              -> ConnectorBoxSpec u
adaptAnaTrail fn = boxConnector $ \props p0 p1 ->
    connectorBoxHalfSize props >>= \sz ->
    let v0    = pvec p0 p1 
        v1    = v0 ^+^ avec (vdirection v0) (2*sz)
        vinit = avec (vdirection v0) (-sz)
    in applyLoc (drawAnaTrail CSTROKE $ fn sz v1) (displace vinit p0)
    


-- | Draw a stroked, rectangular box around the connector points.
--
-- The rectangle will be inclined to the line.
--
conn_box :: (Real u, Floating u, InterpretUnit u) 
        => ConnectorBoxSpec u
conn_box = adaptAnaTrail (\sz -> incline_rect (2 * sz))


-- | Draw a stroked, tube around the connector points.
--
-- The tube will be inclined to the line.
--
conn_tube :: (Real u, Floating u, InterpretUnit u) 
         => ConnectorBoxSpec u
conn_tube = adaptAnaTrail (\sz -> incline_tube (2 * sz))

-- | Draw a stroked, chamfered box around the connector points.
--
-- The tube will be inclined to the line.
--
conn_chamf_box :: (Real u, Floating u, InterpretUnit u) 
               => ConnectorBoxSpec u
conn_chamf_box = adaptAnaTrail (\sz -> incline_chamf_rect (2 * sz))

