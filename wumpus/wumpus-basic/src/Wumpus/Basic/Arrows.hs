{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Arrows
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

module Wumpus.Basic.Arrows
  ( 

    Connector
  , connector
  , leftArrow
  , rightArrow
  , dblArrow
  , leftrightArrow
  , strokeConnector

  -- OLD ...
  , cline


  , arrowTri90
  , arrowTri60
  , arrowTri45
  , arrowOTri90
  , arrowOTri60
  , arrowOTri45

  , arrowBarb90
  , arrowBarb60
  , arrowBarb45
    
  , arrowPerp
  , arrowRBracket

  ) where

import Wumpus.Basic.Arrows.Tips
import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths
import Wumpus.Basic.Utils.Intersection ( langle )

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative
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

strokeConnector :: (Real u, Floating u) 
                => Connector u -> ConnectorImage u (Path u)
strokeConnector (Connector cpF opt_la opt_ra) = \p0 p1 ->
    let pathc = cpF p0 p1 in 
    intoImage (return pathc) (fn pathc opt_la p0 opt_ra p1)
  where
    fn pathc ma p0 mb p1 = let (path1,tipl) = applyTipL ma p0 pathc
                               (path2,tipr) = applyTipR mb p1 path1
                           in maybe mempty (drawF tipl tipr) $ toPrimPath path2

    drawF t1 t2 primpath = (openStroke primpath) `mappend` t1 `mappend` t2
   


applyTipL :: (Real u, Floating u) 
          => Maybe (Arrowhead u) -> Point2 u -> Path u -> (Path u, Graphic u)
applyTipL Nothing    _   pathc = (pathc,mempty)
applyTipL (Just arw) ptL pathc = 
    if dx > 0 then (shortenL dx pathc, grafik) else (pathc, grafik)
  where
    grafik = (arrow_draw arw) (directionL pathc) ptL
    dx     = retract_dist arw

applyTipR :: (Real u, Floating u) 
          => Maybe (Arrowhead u) -> Point2 u -> Path u -> (Path u, Graphic u)
applyTipR Nothing    _   pathc = (pathc,mempty)
applyTipR (Just arw) ptR pathc = 
    if dx > 0 then (shortenR dx pathc, grafik) else (pathc, grafik)
  where
    grafik = (arrow_draw arw) (directionR pathc) ptR 
    dx     = retract_dist arw
   
 
                                     
-- The code below is old and wrong...

liftConnectorPath :: ConnectorPath u -> ConnectorDrawingR u (Path u)
liftConnectorPath pF p1 p2 = pure $ pF p1 p2

cline :: Num u => ConnectorPath u -> ConnectorImage u (Path u)
cline pathF = intoConnectorImage (liftConnectorPath pathF) (pathGraphic pathF)

-- ... No - can\'t a add tips to this one.
--
pathGraphic :: Num u => ConnectorPath u -> ConnectorGraphic u
pathGraphic bpath = \p1 p2 -> openStroke $ toPrimPathU $ bpath p1 p2


-- Here the path is already shortened - we have accounted for the
-- points already, so it is just a graphic. 
lineTipR :: Num u => Path u -> Graphic u -> Graphic u
lineTipR bpath arrtip = openStroke (toPrimPathU bpath) `mappend` arrtip
   



-- | Returns two items:
-- 
-- 1. Shorten the line by the line width - this stops the path
-- tip puncturing the arrow head (particulary visible on open 
-- triangle tips).
-- 
-- 2. Calculate the direction back along the line at half the 
-- lower_x_height - this gets a good angle for the tip on curved
-- path segments.
--
rightPathProps :: (Real u, Floating u, FromPtSize u) 
               => ConnectorPath u -> ConnectorDrawingR u (Path u,Radian)
rightPathProps pathF p1 p2 = 
    (\h sw -> (shortenPath h sw, calcTheta h))
      <$> markHeight <*> lineWidth
  where
    long_path          = pathF p1 p2  
    shortenPath lxh sw = shortenR (lxh + (realToFrac sw)) long_path 
    calcTheta lxh      = directionR $ shortenR (0.5*lxh) long_path




triTipRight :: (Real u, Floating u, FromPtSize u) 
            => ConnectorPath u 
            -> (Radian -> LocGraphic u) 
            -> ConnectorImage u (Path u) 
triTipRight pathF tipF p1 p2 =
    rightPathProps pathF p1 p2          >>= \(shortF,theta) -> 
    lineTipR shortF (tipF theta p2)     >>= \arrow_pic      ->
    return (pathF p1 p2, arrow_pic)




-- This version does not /retract/ the path...
--
barbTipRight :: (Real u, Floating u, FromPtSize u) 
             => ConnectorPath u 
             -> (Radian -> LocGraphic u) 
             -> ConnectorImage u (Path u)  
barbTipRight pathF tipF p1 p2 = 
    rightPathProps pathF p1 p2          >>= \(_,theta) -> 
    lineTipR path_zero (tipF theta p2)  >>= \arrow_pic  ->
    return (path_zero, arrow_pic)
  where
    path_zero = pathF p1 p2



arrowTri90 :: (Real u, Floating u, FromPtSize u) 
           => ConnectorPath u -> ConnectorImage u (Path u)
arrowTri90 pathF = triTipRight pathF tri90
 
          


arrowTri60 :: (Real u, Floating u, FromPtSize u) 
           => ConnectorPath u -> ConnectorImage u (Path u)
arrowTri60 pathF = triTipRight pathF tri60

 
arrowTri45 :: (Real u, Floating u, FromPtSize u) 
           => ConnectorPath u -> ConnectorImage u (Path u)
arrowTri45 pathF = triTipRight pathF tri45


arrowOTri90 :: (Real u, Floating u, FromPtSize u) 
            => ConnectorPath u -> ConnectorImage u (Path u)
arrowOTri90 pathF = triTipRight pathF otri90
     

arrowOTri60 :: (Real u, Floating u, FromPtSize u) 
            => ConnectorPath u -> ConnectorImage u (Path u)
arrowOTri60 pathF = triTipRight pathF otri60 



arrowOTri45 :: (Real u, Floating u, FromPtSize u) 
            => ConnectorPath u -> ConnectorImage u (Path u)
arrowOTri45 pathF = triTipRight pathF otri45



arrowBarb90 :: (Real u, Floating u, FromPtSize u) 
            => ConnectorPath u -> ConnectorImage u (Path u)
arrowBarb90 pathF = barbTipRight pathF barb90

arrowBarb60 :: (Real u, Floating u, FromPtSize u) 
            => ConnectorPath u -> ConnectorImage u (Path u)
arrowBarb60 pathF = barbTipRight pathF barb60

-- ********

arrowBarb45 :: (Real u, Floating u, FromPtSize u) 
            => ConnectorPath u -> ConnectorImage u (Path u)
arrowBarb45 _pathF = undefined -- barbTipRight pathF barb45


                     
arrowPerp :: (Real u, Floating u, FromPtSize u) 
          => ConnectorPath u -> ConnectorImage u (Path u)
arrowPerp pathF p1 p2 = 
    lineTipR path_zero perp_tip >>= \arrow_pic -> return (path_zero, arrow_pic)
  where
    path_zero = pathF  p1 p2
    theta     = langle p1 p2
    perp_tip  = perp theta p2


arrowRBracket :: (Real u, Floating u, FromPtSize u) 
              => ConnectorPath u -> ConnectorImage u (Path u)
arrowRBracket pathF p1 p2 = 
    lineTipR path_zero perp_tip >>= \arrow_pic -> return (path_zero, arrow_pic)
  where
    path_zero = pathF  p1 p2
    theta     = langle p1 p2
    perp_tip  = rbracket theta p2
