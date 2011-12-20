{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Connectors.Base
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

module Wumpus.Drawing.Connectors.Base
  ( 


    ConnectorPathQuery
  , SpacingProjection

  , ArrowTip(..)
  , ArrowConnector

  , ConnectorConfig(..)
  , ConnectorPathSpec(..)
  , renderConnectorConfig

  , arrowDecoratePath

  , leftArrowConnector
  , rightArrowConnector
  , uniformArrowConnector

  ) where

import Wumpus.Drawing.Connectors.ConnectorProps
import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid




type SpacingProjection u = 
      ConnectorProps -> Point2 u -> Point2 u -> Query u (Point2 u)



-- | The type of Connectors - a query from start and end point 
-- returning an AbsPath.
--
type ConnectorPathQuery u = ConnectorQuery u (AbsPath u)



-- | Arrowhead /algorithm/ - the components of an arrowhead.
-- 
-- Retract distance is rather vague - depending on the arrowhead
-- it may represent a flush join between the path and the tip
-- or a join that uses the z-order (tip over path) to create the 
-- join.
--
-- \*\* WARNING \*\* - pending revision...
--
data ArrowTip = ArrowTip
      { retract_distance :: En
      , tip_half_len     :: En
      , tip_deco         :: LocThetaGraphic En
      }


newtype ConnectorPathSpec u = ConnectorPathSpec { 
      getConnectorPathSpec :: ConnectorProps -> ConnectorPathQuery u }

-- | total_path is the path before accounting for arrow 
-- retract distances.
--
data ConnectorConfig u = ConnectorConfig
      { conn_arrowl     :: Maybe ArrowTip
      , conn_arrowr     :: Maybe ArrowTip
      , conn_path_spec  :: ConnectorPathSpec u 
      }



-- Ideally there should be a plus operation to combine tips 
-- allowing double tips.
-- 

type ArrowConnector u = ConnectorImage u (AbsPath u)




-- | NOTE - the prefix /render/ needs (re-) consideration...
-- 
-- If it is a good prefix other functions e.g. drawPath should 
-- use render rather than draw.
--
renderConnectorConfig :: (Real u, Floating u, InterpretUnit u)
                      => ConnectorProps
                      -> ConnectorConfig u
                      -> ConnectorImage u (AbsPath u)
renderConnectorConfig props (ConnectorConfig mbl mbr pspec) = 
    promoteConn $ \src dst -> 
      liftQuery (qapplyConn path_spec src dst) >>= \tot_path -> 
      connectorSrcSpace props >>= \sepl -> 
      connectorDstSpace props >>= \sepr ->
      arrowDecoratePath mbl mbr $ shortenL sepl $ shortenR sepr tot_path
  where
    path_spec = getConnectorPathSpec pspec props



arrowDecoratePath :: (Real u, Floating u, InterpretUnit u) 
                  => Maybe ArrowTip -> Maybe ArrowTip -> (AbsPath u) 
                  -> Image u (AbsPath u)
arrowDecoratePath mbl mbr initial_path = 
      uconvertCtx1 (maybe 0 retract_distance mbl) >>= \retl -> 
      uconvertCtx1 (maybe 0 retract_distance mbr) >>= \retr -> 
      let (p1,theta1)  = atstart initial_path
          (p2,theta2)  = atend   initial_path
          new_path     = shortenL retl $ shortenR retr initial_path
          arrl         = mbTip p1 (pi + theta1) mbl
          arrr         = mbTip p2 theta2 mbr
      in replaceAns initial_path $ 
           decorate ZABOVE (renderPath OSTROKE new_path) (arrl `mappend` arrr)
  where
    mbTip pt ang = maybe emptyImage (supplyLocTheta pt ang . uconvF . tip_deco)


-- | Shorthand...
--
leftArrowConnector :: (Real u, Floating u, InterpretUnit u)
                    => ConnectorProps -> ConnectorPathSpec u -> ArrowTip
                    -> ConnectorImage u (AbsPath u)
leftArrowConnector props cpath tip = renderConnectorConfig props cfg 
  where
    cfg = ConnectorConfig { conn_arrowl    = Just tip 
                          , conn_arrowr    = Nothing
                          , conn_path_spec = cpath }

rightArrowConnector :: (Real u, Floating u, InterpretUnit u)
                    => ConnectorProps -> ConnectorPathSpec u -> ArrowTip
                    -> ConnectorImage u (AbsPath u)
rightArrowConnector props cpath tip = renderConnectorConfig props cfg 
  where
    cfg = ConnectorConfig { conn_arrowl    = Nothing
                          , conn_arrowr    = Just tip
                          , conn_path_spec = cpath }
    
uniformArrowConnector :: (Real u, Floating u, InterpretUnit u)
                    => ConnectorProps -> ConnectorPathSpec u -> ArrowTip
                    -> ConnectorImage u (AbsPath u)
uniformArrowConnector props cpath tip = renderConnectorConfig props cfg 
  where
    cfg = ConnectorConfig { conn_arrowl    = Just tip
                          , conn_arrowr    = Just tip
                          , conn_path_spec = cpath }
    

