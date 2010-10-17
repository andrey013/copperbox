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

import Control.Applicative

-- An arrowhead always know how to draw itself (filled triangle, 
-- stroked barb, etc.)
--
-- A Path might will typically be drawn with openStroke,
-- eventually there might be scope for drawing 
-- e.g. parallel lines  ====
--

-- A ConnectorPath gets wrapped with how it is drawn into
-- another type.


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



-- Actually this is wrong.
-- It is not shortening the drawn path... 

strokeConnector :: (Real u, Floating u) 
                => Connector u -> ConnectorImage u (Path u)
strokeConnector (Connector cpF opt_la opt_ra) p0 p1 =
    feed pathc trafo (openStroke . toPrimPath)
  where
    pathc       = cpF p0 p1
    trafL       = tipTrafo opt_la shortenL (directionL pathc) p0
    trafR       = tipTrafo opt_ra shortenR (directionR pathc) p1
    trafo       = trafL `combineImageTrafo` trafR
   



-- Note - feed returns the original @a@, but draws with the 
-- modified @a'@, so this is very much a special case combinator
-- for drawing arrows.
--
feed :: a -> ImageTrafoF u a -> (a -> Graphic u) -> Image u a
feed a trf mk = 
    trf >>= \(fa,fg) -> let a' = fa a in intoImage (pure a) (fg <$> mk a')


tipTrafo :: (Real u, Floating u) 
         => Maybe (Arrowhead u) -> (u -> Path u -> Path u) 
         -> Radian -> Point2 u -> ImageTrafoF u (Path u)
tipTrafo Nothing    _        _     _  = intoImageTrafo (pure id) unmarked
tipTrafo (Just arw) shortenF theta pt = 
    getArrowhead arw theta pt >>= \(dx,prim) -> 
    if dx > 0 then intoImageTrafo (pure $ shortenF dx) (superiorPrim prim)
              else intoImageTrafo (pure id) (superiorPrim prim)
  where
    superiorPrim = superiorGraphic . pure



unmarked :: GraphicTrafoF u
unmarked = pure id

