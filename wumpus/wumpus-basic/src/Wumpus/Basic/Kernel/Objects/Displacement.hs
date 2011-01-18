{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Displacement
-- Copyright   :  (c) Stephen Tetley 2010
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


  -- * Moving points
    PointDisplace
  , ThetaPointDisplace


  , moveStartPoint
  , moveStartPointTheta


  , displace
  , displaceVec
  , displaceH
  , displaceV

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


  , thetaNorthwards
  , thetaSouthwards 
  , thetaEastwards
  , thetaWestwards  

  , thetaNortheastwards
  , thetaNorthwestwards
  , thetaSoutheastwards
  , thetaSouthwestwards

  ) where


import Wumpus.Basic.Kernel.Base.ContextFun

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space



--------------------------------------------------------------------------------
-- Displacing points

-- | 'PointDisplace' is a type representing functions 
-- @from Point to Point@.
--
-- It is especially useful for building composite graphics where 
-- one part of the graphic is drawn from a different start point 
-- to the other part.
--
type PointDisplace u = Point2 u -> Point2 u


-- | 'ThetaPointDisplace' is a type representing functions 
-- @from Radian * Point to Point@.
--
-- It is useful for building arrowheads which are constructed 
-- with an implicit angle representing the direction of the line 
-- at the arrow tip.
--
type ThetaPointDisplace u = Radian -> PointDisplace u



-- | Move the start-point of a LocImage with the supplied 
-- displacement function.
--
moveStartPoint :: PointDisplace u -> LocCF u a -> LocCF u a
moveStartPoint f ma = promoteR1 $ \pt -> apply1R1 ma (f pt)


-- | Move the start-point of a LocImage with the supplied 
-- displacement function.
--
moveStartPointTheta :: PointDisplace u -> LocThetaCF u a -> LocThetaCF u a
moveStartPointTheta f ma = promoteR2 $ \pt theta -> apply2R2 ma (f pt) theta


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
-- direction of the implicit angle by the supplied distance 
-- @dist@. 
--
displacePerpendicular :: Floating u => u -> ThetaPointDisplace u
displacePerpendicular d = 
    \theta pt -> pt .+^ avec (circularModulo $ theta + (0.5*pi)) d




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

