{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Connectors.ConnectorProps
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Connectors...
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Connectors.ConnectorProps
  ( 

    -- * Data types
    ConnectorProps(..)
  , default_connector_props

  -- * Queries
  , connectorSrcSpace
  , connectorDstSpace
  , connectorArms
  , connectorLoopSize

  ) where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative



-- | ConnectorProps control the drawing of connectors in 
-- Wumpus-Drawing.
--
-- > conn_src_space     :: Em
-- > conn_dst_space     :: Em
--
-- Source and destination offsets - these offset the drawing of
-- the connector perpendicular to the direction of line formed 
-- between the connector points (a positive offset is drawn above, 
-- a negative offset below). The main use of offsets is to draw
-- parallel line connectors.
--
-- > conn_arc_ang       :: Radian 
--
-- Control the /bend/ of an arc connector.
-- 
-- > conn_src_arm       :: Em
-- > conn_dst_arm       :: Em 
--
-- Control the /arm/ length of a jointed connector - arms are the 
-- initial segments of the connector. 
--
-- > conn_loop_size     :: Em
--
-- Control the /height/ of a loop connector. 
--
-- > conn_box_halfsize  :: Em
-- 
-- Control the size of a connector box. Connector boxes are 
-- drawn with the exterior lines projected out from the connector
-- points a halfsize above and below.
-- 
data ConnectorProps = ConnectorProps
      { conn_src_space       :: !Em
      , conn_dst_space       :: !Em
      , conn_arc_ang         :: !Radian
      , conn_src_arm         :: !Em
      , conn_dst_arm         :: !Em
      , conn_loop_size       :: !Em
      , conn_box_halfsize    :: !Em
      }



-- | Default connector properties.
--
-- > conn_src_sep:        0
-- > conn_dst_sep:        0
-- > conn_arc_ang:        pi / 12
-- > conn_src_arm:        1
-- > conn_dst_arm:        1
-- > conn_loop_size:      2 
-- > conn_box_half_size:  2
--
-- Arc angle is 15deg - quite shallow.
--
default_connector_props :: ConnectorProps
default_connector_props = 
    ConnectorProps { conn_src_space    = 0
                   , conn_dst_space    = 0
                   , conn_arc_ang      = pi / 12
                   , conn_src_arm      = 1
                   , conn_dst_arm      = 1
                   , conn_loop_size    = 2 
                   , conn_box_halfsize = 2 
                   }




--------------------------------------------------------------------------------
-- Queries


connectorSrcSpace :: (DrawingCtxM m, InterpretUnit u) 
                  => ConnectorProps -> m u
connectorSrcSpace props = 
    (\sz -> uconvert1 sz $ conn_src_space props) <$> pointSize


connectorDstSpace :: (DrawingCtxM m, InterpretUnit u) 
                  => ConnectorProps -> m u
connectorDstSpace props = 
    (\sz -> uconvert1 sz $ conn_src_space props) <$> pointSize


connectorArms :: (DrawingCtxM m, InterpretUnit u) 
              => ConnectorProps -> m (u,u)
connectorArms props = 
    (\sz -> ( uconvert1 sz $ conn_src_arm props
            , uconvert1 sz $ conn_dst_arm props) )
        <$> pointSize



connectorLoopSize :: (DrawingCtxM m, InterpretUnit u) 
                  => ConnectorProps -> m u
connectorLoopSize props = 
    (\sz -> uconvert1 sz $ conn_loop_size props) <$> pointSize

