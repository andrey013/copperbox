{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Arrows.Connectors
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

module Wumpus.Drawing.Arrows.Connectors
  ( 

    Connector
  , connector
  , leftArrow
  , rightArrow
  , dblArrow
  , leftrightArrow
  , strokeConnector


  ) where

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Arrows.Tips
import Wumpus.Drawing.Paths

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


intoImage :: CF a -> CF (z,b) -> CF (a,b)
intoImage = liftA2 (\a (_,b) -> (a,b))

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a,f b)

strokeConnector :: (Real u, Floating u) 
                => Connector u -> ConnectorImage u (Path u)
strokeConnector (Connector cpF opt_la opt_ra) =
    promoteR2 $ \p0 p1 -> let pathc = cpF p0 p1 in 
       tipEval opt_la p0 (directionL pathc) >>= \(dl,gfL) -> 
       tipEval opt_ra p1 (directionR pathc) >>= \(dr,gfR) ->
       intoImage (pure pathc) 
                 (fmap (mapSnd (gfR . gfL)) $ drawP $ shortenPath dl dr pathc) 
  where
    drawP       = openStroke . toPrimPath   


-- for Paths.Base ?
--
shortenPath :: (Real u , Floating u) => u  -> u -> Path u -> Path u
shortenPath l r = shortenL l .  shortenR r 

type ArrowMark u = PrimGraphic u -> PrimGraphic u


-- 'tipEval' is a bit of an oddity. It has to evaluate the 
-- Arrowhead / Image in the DrawingCtx to get the retract 
-- distance. But doing so evaluates the tips to PrimGraphics, thus 
-- it has to wrap the tips back up as Graphics with @pure@ so they 
-- can be concatenated to the drawn path as GraphicTrafos.
--
-- The Arrowhead type could be changed, so rather than returning 
-- an Image (retract_distance, PrimGraphic) it returns 
-- (retract_distance, GraphicTrafo) but that would burden all 
-- arrowheads with some extra complexity.
-- 
-- In short - the code here works but it isn\'t exemplary, and it 
-- doesn\'t show whether or not GraphicTrafo is a valuable type or
-- if it is implemented correctly (as GraphicTrafo could having
-- different implementations according to how it regards the 
-- DrawingCtx).
--

tipEval :: Num u 
        => Maybe (Arrowhead u) -> Point2 u -> Radian
        -> CF (u, ArrowMark u)
tipEval Nothing    _  _     = return (0,unmarked)
tipEval (Just arw) pt theta = makeMark $ down2R2 pt theta (getArrowhead arw)


unmarked :: ArrowMark u
unmarked = id


makeMark :: Image u a -> CF (a, ArrowMark u)
makeMark = fmap (\(a,prim) -> (a, (`oplus` prim)))



