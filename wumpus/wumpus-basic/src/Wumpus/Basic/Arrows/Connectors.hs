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
import Wumpus.Basic.Utils.Combinators

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative

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


strokeConnector_OLD :: (Real u, Floating u) 
                => Connector u -> ConnectorImage u (Path u)
strokeConnector_OLD (Connector cpF opt_la opt_ra) = \p0 p1 ->
    let pathc = cpF p0 p1 in 
    (fn pathc opt_la p0 opt_ra p1) >>= \grafic ->
    intoImage (return pathc) grafic
  where
    fn pathc ma p0 mb p1 = do 
       (path1,tipl) <- applyTipL ma p0 pathc
       (path2,tipr) <- applyTipR mb p1 path1
       return $ (tipr $ tipl $ openStroke $ toPrimPath path2)
   

-- OUT_OF_DATE
type GraphicF u = Graphic u -> Graphic u

unmarked :: GraphicF u
unmarked = id

applyTipL :: (Real u, Floating u) 
          => Maybe (Arrowhead u) -> Point2 u -> Path u 
          -> DrawingR (Path u, GraphicF u)
applyTipL Nothing    _   pathc = return (pathc,id)
applyTipL (Just arw) ptL pathc = 
    retractDistance arw >>= \ dx -> 
    if dx > 0 then return (shortenL dx pathc, (superior grafik) )
              else return (pathc, (superior grafik))
  where
    grafik = arrowheadLocGraphic arw (directionL pathc) ptL


applyTipR :: (Real u, Floating u) 
          => Maybe (Arrowhead u) -> Point2 u -> Path u 
          -> DrawingR (Path u, GraphicF u)
applyTipR Nothing    _  pathc = return (pathc, unmarked)
applyTipR (Just arw) pt pathc = 
    retractDistance arw >>= \dx -> 
    if dx > 0 then return (shortenR dx pathc, (superior grafik))
              else return (pathc, (superior grafik))
  where
    grafik = arrowheadLocGraphic arw (directionR pathc) pt

-- Whoa - applyTipR returns a modified path AND a graphic modifier!
-- This seems like mixed tense



strokeConnector :: (Real u, Floating u) 
                => Connector u -> ConnectorImage u (Path u)
strokeConnector (Connector cpF opt_la opt_ra) p0 p1 =
    let trafL = tipTrafo opt_la (directionL pathc) p0
        trafR = tipTrafo opt_ra (directionR pathc) p1
    in applyImageTransformer trafR $ applyImageTransformer trafL 
                                   $ mkPathImg pathc
  where
    pathc       = cpF p0 p1
    mkPathImg a = intoImage (pure a) (openStroke $ toPrimPath a)
   



tipTrafo :: (Real u, Floating u) 
       => Maybe (Arrowhead u) -> Radian -> Point2 u -> ImageTransformerF u (Path u)
tipTrafo Nothing    _     _  = intoImageTransformerF (pure id) unmarkedF
tipTrafo (Just arw) theta pt = 
    retractDistance arw >>= \dx -> 
    if dx > 0 then intoImageTransformerF (pure $ shortenR dx) gtrafo
              else intoImageTransformerF (pure id) gtrafo
  where
    gtrafo = superiorGraphic $ arrowheadLocGraphic arw theta pt



unmarkedF :: GraphicTransformerF u
unmarkedF = pure id

