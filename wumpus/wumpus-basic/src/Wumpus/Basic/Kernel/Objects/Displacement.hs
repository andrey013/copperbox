{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Displacement
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Displacing points - often start points. 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Displacement
  (

    PointDisplace
  , ThetaDisplace
  , ThetaPointDisplace

  , moveStart
  , moveStartTheta
  , moveStartThetaPoint
  , moveStartThetaIncl

  , displace
  , displaceVec
  , displaceH
  , displaceV

  , move_up
  , move_down
  , move_left
  , move_right

  , northwards
  , southwards 
  , eastwards
  , westwards  

  , northeastwards
  , northwestwards
  , southeastwards
  , southwestwards


  , displaceParallel
  , displacePerpendicular
  , displaceOrtho

  , thetaNorthwards
  , thetaSouthwards 
  , thetaEastwards
  , thetaWestwards  

  , thetaNortheastwards
  , thetaNorthwestwards
  , thetaSoutheastwards
  , thetaSouthwestwards

  , centerRelative
  , left_of
  , right_of
  , above_left_of
  , above_right_of
  , below_left_of
  , below_right_of

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.Anchors

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space




-- | 'PointDisplace' is a type representing functions 
-- @from Point to Point@.
--
-- It is especially useful for building composite graphics where 
-- one part of the graphic is drawn from a different start point 
-- to the other part.
--
type PointDisplace u = Point2 u -> Point2 u




-- | 'ThetaDisplace' is a type representing functions 
-- @from Radian to Radian@.
--
-- It is especially useful for building composite graphics where 
-- one part of the graphic is drawn from a different start point 
-- to the other part.
--
type ThetaDisplace = Radian -> Radian


-- | 'ThetaPointDisplace' is a type representing functions 
-- @from Radian * Point to Point@.
--
-- It is useful for building arrowheads which are constructed 
-- with an implicit angle representing the direction of the line 
-- at the arrow tip.
--
type ThetaPointDisplace u = Radian -> Point2 u -> Point2 u


--------------------------------------------------------------------------------
-- Displacing points and inclination


-- | Move the start-point of a 'LocQuery' with the supplied 
-- displacement function.
--
moveStart :: PointDisplace u -> LocQuery u a -> LocQuery u a
moveStart f ma = promoteR1 $ \pt -> apply1R1 ma (f pt)



-- | Move the start-point of a 'LocThetaQuery' with the supplied 
-- displacement function.
--
moveStartTheta :: ThetaPointDisplace u 
               -> LocThetaQuery u a -> LocThetaQuery u a
moveStartTheta f ma = promoteR2 $ \pt theta -> let p2 = f theta pt 
                                               in apply2R2 ma p2 theta


-- | Move the start-point of a 'LocThetaCF' with the supplied 
-- displacement function.
--
moveStartThetaPoint :: PointDisplace u 
                    -> LocThetaQuery u a -> LocThetaQuery u a
moveStartThetaPoint f ma = promoteR2 $ \pt theta -> apply2R2 ma (f pt) theta


-- | Change the inclination of a 'LocThetaCF' with the supplied 
-- displacement function.
--
moveStartThetaIncl :: ThetaDisplace -> LocThetaQuery u a -> LocThetaQuery u a
moveStartThetaIncl f ma = promoteR2 $ \pt theta -> apply2R2 ma pt (f theta)


--------------------------------------------------------------------------------
-- PointDisplace functions

-- | 'displace' : @ x -> y -> PointDisplace @
--
-- Build a combinator to move @Points@ by the supplied @x@ and 
-- @y@ distances.
--
displace :: Num u => u -> u -> PointDisplace u
displace dx dy (P2 x y) = P2 (x+dx) (y+dy)


-- | 'displaceV' : @ (V2 x y) -> PointDisplace @
-- 
-- Version of 'displace' where the displacement is supplied as
-- a vector rather than two parameters.
-- 
displaceVec :: Num u => Vec2 u -> PointDisplace u
displaceVec (V2 dx dy) (P2 x y) = P2 (x+dx) (y+dy)


-- | 'displaceH' : @ x -> PointDisplace @
-- 
-- Build a combinator to move @Points@ by horizontally the 
-- supplied @x@ distance.
--
displaceH :: Num u => u -> PointDisplace u
displaceH dx (P2 x y) = P2 (x+dx) y

-- | 'displaceV' : @ y -> PointDisplace @
-- 
-- Build a combinator to move @Points@ vertically by the supplied 
-- @y@ distance.
--
displaceV :: Num u => u -> PointDisplace u
displaceV dy (P2 x y) = P2 x (y+dy)




-- Possibly for Displacement @move_down@ seems a more attrctive 
-- name than @southwards@.
--
move_up :: Num u => u -> PointDisplace u
move_up = displaceV

move_down :: Num u => u -> PointDisplace u
move_down = displaceV . negate

move_left :: Num u => u -> PointDisplace u
move_left = displaceH . negate

move_right :: Num u => u -> PointDisplace u
move_right = displaceH




-- Cardinal displacement 



northwards :: Num u => u -> PointDisplace u
northwards = displaceV


southwards :: Num u => u -> PointDisplace u
southwards =  displaceV . negate

eastwards :: Num u => u -> PointDisplace u
eastwards = displaceH

westwards :: Num u => u -> PointDisplace u
westwards = displaceH . negate

northeastwards :: Floating u => u -> PointDisplace u
northeastwards = displaceVec . avec (0.25 * pi)

northwestwards ::  Floating u => u -> PointDisplace u
northwestwards = displaceVec . avec (0.75 * pi)

southeastwards ::  Floating u => u -> PointDisplace u
southeastwards = displaceVec . avec (1.75 * pi)

southwestwards ::  Floating u => u -> PointDisplace u
southwestwards = displaceVec . avec (1.25 * pi)


--------------------------------------------------------------------------------
-- ThetaPointDisplace functions


-- | 'displaceParallel' : @ dist -> ThetaPointDisplace @
-- 
-- Build a combinator to move @Points@ in parallel to the 
-- direction of the implicit angle by the supplied distance 
-- @dist@. 
--
displaceParallel :: Floating u => u -> ThetaPointDisplace u
displaceParallel d = \theta pt -> pt .+^ avec (circularModulo theta) d


-- | 'displaceParallel' : @ dist -> ThetaPointDisplace @
-- 
-- Build a combinator to move @Points@ perpendicular to the 
-- inclnation of the implicit angle by the supplied distance 
-- @dist@. 
--
displacePerpendicular :: Floating u => u -> ThetaPointDisplace u
displacePerpendicular d = 
    \theta pt -> pt .+^ avec (circularModulo $ theta + (0.5*pi)) d



-- | 'displaceOrtho' : @ vec -> ThetaPointDisplace @
-- 
-- This is a combination of @displaceParallel@ and 
-- @displacePerpendicular@, with the x component of the vector
-- displaced in parallel and the y component displaced
-- perpendicular. 
-- 
displaceOrtho :: Floating u => Vec2 u -> ThetaPointDisplace u
displaceOrtho (V2 x y) = \theta -> 
    displaceParallel x theta . displacePerpendicular y theta


thetaNorthwards :: Floating u => u -> ThetaPointDisplace u
thetaNorthwards = displacePerpendicular


thetaSouthwards :: Floating u => u -> ThetaPointDisplace u
thetaSouthwards = displacePerpendicular . negate


thetaEastwards :: Floating u => u -> ThetaPointDisplace u
thetaEastwards = displaceParallel


thetaWestwards :: Floating u => u -> ThetaPointDisplace u
thetaWestwards = displaceParallel . negate

thetaNortheastwards :: Floating u => u -> ThetaPointDisplace u
thetaNortheastwards d = 
    \theta pt -> pt .+^ avec (circularModulo $ theta + (0.25*pi)) d


thetaNorthwestwards :: Floating u => u -> ThetaPointDisplace u
thetaNorthwestwards d = 
    \theta pt -> pt .+^ avec (circularModulo $ theta + (0.75*pi)) d


thetaSoutheastwards :: Floating u => u -> ThetaPointDisplace u
thetaSoutheastwards d = 
    \theta pt -> pt .+^ avec (circularModulo $ theta + (1.75*pi)) d


thetaSouthwestwards :: Floating u => u -> ThetaPointDisplace u
thetaSouthwestwards d = 
    \theta pt -> pt .+^ avec (circularModulo $ theta + (1.25*pi)) d



--------------------------------------------------------------------------------

-- | Absolute units.
-- 
centerRelative :: (CenterAnchor t u, Fractional u, InterpretUnit u) 
               => (Int,Int) -> t u -> Anchor u
centerRelative coord a =
   center a >>= \pt ->  (snapmove coord) >>= \v -> return (pt .+^ v)


-- | Absolute units.
--
right_of        :: (CenterAnchor t u, Fractional u, InterpretUnit u) 
                => t u -> Anchor u
right_of        = centerRelative (1,0)

-- | Absolute units.
--
left_of         :: (CenterAnchor t u, Fractional u, InterpretUnit u) 
                => t u -> Anchor u
left_of         = centerRelative ((-1),0)

-- | Absolute units.
--
above_right_of  :: (CenterAnchor t u, Fractional u, InterpretUnit u) 
                => t u -> Anchor u
above_right_of  = centerRelative (1,1)

-- | Absolute units.
--
below_right_of  :: (CenterAnchor t u, Fractional u, InterpretUnit u) 
                => t u -> Anchor u
below_right_of  = centerRelative (1, (-1))

-- | Absolute units.
--
above_left_of   :: (CenterAnchor t u, Fractional u, InterpretUnit u) 
                => t u -> Anchor u
above_left_of   = centerRelative ((-1),1)

-- | Absolute units.
--
below_left_of   :: (CenterAnchor t u, Fractional u, InterpretUnit u) 
                => t u -> Anchor u
below_left_of   = centerRelative ((-1),(-1))
 




