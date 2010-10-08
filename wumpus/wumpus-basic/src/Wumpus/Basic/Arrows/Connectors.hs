{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Arrows.Connectors
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Draw arrows.
--
-- \*\* WARNING \*\* - the types are /wrong/ here and need more 
-- thought.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Arrows.Connectors
  ( 

    Connector
  , connector
  , leftArrow
  , rightArrow
  , dblArrow
  , leftrightArrow
  , strokeConnector


  ) where

import Wumpus.Basic.Arrows.Tips
import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths

import Wumpus.Core                      -- package: wumpus-core

import Data.Monoid

-- An arrowhead always know how to draws itself (filled tri, 
-- stroked barb, etc.)
--
-- A Path might will typically be drawn with openStroke,
-- eventually there might be scope for drawing 
-- e.g. parallel lines  ====
--

-- A ConnectorPath gets wrapped with how it is drawn into
-- another type.


-- larrow :: Arrowhead u -> ConnectorPath u -> WrappedConnector u


data Connector u = Connector 
      { connector_path  :: ConnectorPath u
      , opt_left_arrow  :: Maybe (Arrowhead u)
      , opt_right_arrow :: Maybe (Arrowhead u)
      }


-- | connector with no arrow heads.
--
connector :: ConnectorPath u -> Connector u
connector cp = 
    Connector { connector_path  = cp
              , opt_left_arrow  = Nothing
              , opt_right_arrow = Nothing
              }

leftArrow :: ConnectorPath u -> Arrowhead u -> Connector u
leftArrow cp la =
    Connector { connector_path  = cp
              , opt_left_arrow  = Just la
              , opt_right_arrow = Nothing
              }


rightArrow :: ConnectorPath u -> Arrowhead u -> Connector u
rightArrow cp ra = 
    Connector { connector_path  = cp
              , opt_left_arrow  = Nothing
              , opt_right_arrow = Just ra
              }

-- | Same tip both ends.
--
dblArrow :: ConnectorPath u -> Arrowhead u -> Connector u
dblArrow cp arw = leftrightArrow cp arw arw

leftrightArrow :: ConnectorPath u -> Arrowhead u -> Arrowhead u -> Connector u
leftrightArrow cp la ra =
    Connector { connector_path  = cp
              , opt_left_arrow  = Just la
              , opt_right_arrow = Just ra
              }

-- NEEDS TIDYING...

strokeConnector :: (Real u, Floating u) 
                => Connector u -> ConnectorImage u (Path u)
strokeConnector (Connector cpF opt_la opt_ra) = \p0 p1 ->
    let pathc = cpF p0 p1 in 
    (fn pathc opt_la p0 opt_ra p1) >>= \grafic ->
    intoImage (return pathc) grafic
  where
    fn pathc ma p0 mb p1 = do 
       (path1,tipl) <- applyTipL ma p0 pathc
       (path2,tipr) <- applyTipR mb p1 path1
       return $ maybe mempty (drawF tipl tipr) $ toPrimPath path2

    drawF t1 t2 primpath = (openStroke primpath) `mappend` t1 `mappend` t2
   


applyTipL :: (Real u, Floating u) 
          => Maybe (Arrowhead u) -> Point2 u -> Path u 
          -> DrawingR (Path u, Graphic u)
applyTipL Nothing    _   pathc = return (pathc,mempty)
applyTipL (Just arw) ptL pathc = 
    retract_dist arw >>= \ dx -> 
    if dx > 0 then return (shortenL dx pathc, grafik) 
              else return (pathc, grafik)
  where
    grafik = (arrow_draw arw) (directionL pathc) ptL

applyTipR :: (Real u, Floating u) 
          => Maybe (Arrowhead u) -> Point2 u -> Path u 
          -> DrawingR (Path u, Graphic u)
applyTipR Nothing    _   pathc = return (pathc,mempty)
applyTipR (Just arw) ptR pathc = 
    retract_dist arw >>= \dx -> 
    if dx > 0 then return (shortenR dx pathc, grafik) 
              else return (pathc, grafik)
  where
    grafik = (arrow_draw arw) (directionR pathc) ptR 

