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
  , connbox
  , conntube

  ) where

-- import Wumpus.Drawing.Basis.InclineTrails
import Wumpus.Drawing.Connectors.ConnectorProps

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.Monoid

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


-- | Draw a stroked, rectangular box around the connector points.
--
-- The rectangle will be inclined to the line.
--
connbox :: (Real u, Floating u, InterpretUnit u) 
        => ConnectorBoxSpec u
connbox = boxConnector $ \props p0 p1 -> 
    connectorBoxHalfSize props >>= \sz ->
    applyLoc (drawAnaTrail CSTROKE $ cfconnbox sz (pvec p0 p1)) p0


conntube :: (Real u, Floating u, InterpretUnit u) 
         => ConnectorBoxSpec u
conntube = boxConnector $ \props p0 p1 -> 
    connectorBoxHalfSize props >>= \sz ->
    applyLoc (drawAnaTrail CSTROKE $ cfconntube sz (pvec p0 p1)) p0



-- Note can\'t use @incline_rect as does not accommodate spacing.

-- | @v1@ is the /interior/ vector.
--
cfconnbox :: (Real u, Floating u) => u -> Vec2 u -> AnaTrail u
cfconnbox du v1 = 
    anaCatTrail (orthoVec (-du) (-du) ang) $ mconcat $
      [ trail_theta_right w ang
      , trail_theta_up h ang
      , trail_theta_left w ang
      , trail_theta_down h ang
      ]
  where
    ang = vdirection v1 
    w   = (2*du) + vlength v1
    h   = 2*du
    


-- | @v1@ is the /interior/ vector.
--
cfconntube :: (Real u, Floating u) => u -> Vec2 u -> AnaTrail u
cfconntube du v1 = 
    anaCatTrail (orthoVec 0 (-du) ang) $ mconcat $
      [ trail_theta_right w ang
      , semicircleTrail CCW vup
      , trail_theta_left w ang
      , semicircleTrail CCW vdown
      ]
  where
    ang   = vdirection v1 
    w     = vlength v1
    vup   = avec (ang + half_pi) (2*du)
    vdown = avec (ang - half_pi) (2*du)

    


