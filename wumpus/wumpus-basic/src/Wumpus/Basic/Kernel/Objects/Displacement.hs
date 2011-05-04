{-# LANGUAGE TypeFamilies               #-}
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
  , dispVec
  , dispH
  , dispV

  , dispDirection
  , dispCorner
  , dispCardinal


  , dispParallel
  , dispPerpendicular
  , dispOrtho

  , dispDirectionTheta
  , dispCardinalTheta

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
import Wumpus.Basic.Kernel.Objects.Basis

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
moveStart f ma = domMap1 f ma



-- | Move the start-point of a 'LocThetaQuery' with the supplied 
-- displacement function.
--
moveStartTheta :: ThetaPointDisplace u 
               -> LocThetaQuery u a -> LocThetaQuery u a
moveStartTheta f cf = consCF $ \ctx -> 
    (\pt ang -> let f1 = runCF ctx cf in f1 (f ang pt) ang)


-- | Move the start-point of a 'LocThetaCF' with the supplied 
-- displacement function.
--
moveStartThetaPoint :: PointDisplace u 
                    -> LocThetaQuery u a -> LocThetaQuery u a
moveStartThetaPoint f = domMap2 f id


-- | Change the inclination of a 'LocThetaCF' with the supplied 
-- displacement function.
--
moveStartThetaIncl :: ThetaDisplace -> LocThetaQuery u a -> LocThetaQuery u a
moveStartThetaIncl f = domMap2 id f


--------------------------------------------------------------------------------
-- PointDisplace functions

-- | 'displace' : @ x -> y -> PointDisplace @
--
-- Build a combinator to move @Points@ by the supplied @x@ and 
-- @y@ distances.
--
displace :: Num u => u -> u -> PointDisplace u
displace dx dy (P2 x y) = P2 (x+dx) (y+dy)


-- | 'dispVec' : @ (V2 x y) -> PointDisplace @
-- 
-- Version of 'displace' where the displacement is supplied as
-- a vector rather than two parameters.
-- 
dispVec :: Num u => Vec2 u -> PointDisplace u
dispVec (V2 dx dy) (P2 x y) = P2 (x+dx) (y+dy)


-- | 'dispH' : @ x -> PointDisplace @
-- 
-- Build a combinator to move @Points@ by horizontally the 
-- supplied @x@ distance.
--
dispH :: Num u => u -> PointDisplace u
dispH dx (P2 x y) = P2 (x+dx) y

-- | 'dispV' : @ y -> PointDisplace @
-- 
-- Build a combinator to move @Points@ vertically by the supplied 
-- @y@ distance.
--
dispV :: Num u => u -> PointDisplace u
dispV dy (P2 x y) = P2 x (y+dy)

-- 

dispDirection :: Num u => Direction -> u -> PointDisplace u
dispDirection UP     = dispV
dispDirection DOWN   = dispV . negate
dispDirection LEFT   = dispH . negate
dispDirection RIGHT  = dispH   




-- | For the /corner/ cardinals (north-east, etc.)
-- this displaces horizontally and vertically by the supplied 
-- unit distance - i.e. displacing to the corner of the bounding 
-- square.
--
-- Use 'dispCardinal' to displace with the radial length.
-- 
dispCorner :: Num u => Cardinal -> u -> PointDisplace u
dispCorner NORTH      du = dispVec $ V2   0     du
dispCorner NORTH_EAST du = dispVec $ V2   du    du
dispCorner EAST       du = dispVec $ V2   du     0
dispCorner SOUTH_EAST du = dispVec $ V2   du  (-du)
dispCorner SOUTH      du = dispVec $ V2    0  (-du)
dispCorner SOUTH_WEST du = dispVec $ V2 (-du) (-du)
dispCorner WEST       du = dispVec $ V2 (-du)    0
dispCorner NORTH_WEST du = dispVec $ V2 (-du)   du


-- | Displace radially in the supplied direction. 
-- 
-- The supplied distance is the length of radius of a bounding 
-- circle.
--
dispCardinal :: Floating u => Cardinal -> u -> PointDisplace u
dispCardinal NORTH      = dispV
dispCardinal NORTH_EAST = dispVec . avec (0.25 * pi)
dispCardinal EAST       = dispH . negate
dispCardinal SOUTH_EAST = dispVec . avec (1.75 * pi)
dispCardinal SOUTH      = dispV . negate
dispCardinal SOUTH_WEST = dispVec . avec (1.25 * pi)
dispCardinal WEST       = dispH
dispCardinal NORTH_WEST = dispVec . avec (0.75 * pi)


--------------------------------------------------------------------------------
-- ThetaPointDisplace functions


-- | 'dispParallel' : @ dist -> ThetaPointDisplace @
-- 
-- Build a combinator to move @Points@ in parallel to the 
-- direction of the implicit angle by the supplied distance 
-- @dist@. 
--
dispParallel :: Floating u => u -> ThetaPointDisplace u
dispParallel d = \theta pt -> pt .+^ avec (circularModulo theta) d


-- | 'dispParallel' : @ dist -> ThetaPointDisplace @
-- 
-- Build a combinator to move @Points@ perpendicular to the 
-- inclnation of the implicit angle by the supplied distance 
-- @dist@. 
--
dispPerpendicular :: Floating u => u -> ThetaPointDisplace u
dispPerpendicular d = 
    \theta pt -> pt .+^ avec (circularModulo $ theta + (0.5*pi)) d



-- | 'dispOrtho' : @ vec -> ThetaPointDisplace @
-- 
-- This is a combination of @displaceParallel@ and 
-- @displacePerpendicular@, with the x component of the vector
-- displaced in parallel and the y component displaced
-- perpendicular. 
-- 
dispOrtho :: Floating u => Vec2 u -> ThetaPointDisplace u
dispOrtho (V2 x y) = \theta -> dispParallel x theta . dispPerpendicular y theta




-- | /Angular/ version of 'dispDirection'. 
--
-- The displacement direction is with respect to implicit angle
-- of inclination, so:
--
-- > up    == perpendicular
-- > down  == perdendicular . negate
-- > left  == parallel . negate
-- > right == parallel
-- 
dispDirectionTheta :: Floating u => Direction -> u -> ThetaPointDisplace u
dispDirectionTheta UP      = dispPerpendicular
dispDirectionTheta DOWN    = dispPerpendicular . negate
dispDirectionTheta LEFT    = dispParallel . negate
dispDirectionTheta RIGHT   = dispParallel


-- | /Angular/ version of 'dispCardinal'.
--
-- The displacement direction is with respect to implicit angle
-- of inclination, so:
--
-- > north == perpendicular
-- > east  == parallel
-- > south == perdendicular . negate
-- > etc.
-- 
dispCardinalTheta :: Floating u => Cardinal -> u -> ThetaPointDisplace u
dispCardinalTheta NORTH      = dispPerpendicular
dispCardinalTheta NORTH_EAST = \d ang -> dispVec (avec (ang + (0.25*pi)) d)
dispCardinalTheta EAST       = dispParallel
dispCardinalTheta SOUTH_EAST = \d ang -> dispVec (avec (ang + (1.75*pi)) d)
dispCardinalTheta SOUTH      = dispPerpendicular . negate
dispCardinalTheta SOUTH_WEST = \d ang -> dispVec (avec (ang + (1.25*pi)) d)
dispCardinalTheta WEST       = dispParallel . negate
dispCardinalTheta NORTH_WEST = \d ang -> dispVec (avec (ang + (0.75*pi)) d)




--------------------------------------------------------------------------------

-- | Absolute units.
-- 
centerRelative :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
               => (Int,Int) -> a -> Query (Anchor u)
centerRelative coord a = snapmove coord >>= \v -> return $ center a .+^ v

-- TODO - These are really for Anchors.
--
-- Should the have a separate module or be rolled into the same
-- module as the classes?
--

-- | Value is 1 snap unit right.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
right_of        :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query (Anchor u)
right_of        = centerRelative (1,0)

-- | Value is 1 snap move left.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
left_of         :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query (Anchor u)
left_of         = centerRelative ((-1),0)

-- | Value is 1 snap move up, 1 snap move right.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
above_right_of  :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query (Anchor u)
above_right_of  = centerRelative (1,1)

-- | Value is 1 snap move below, 1 snap move right.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
below_right_of  :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query (Anchor u)
below_right_of  = centerRelative (1, (-1))

-- | Value is 1 snap move up, 1 snap move left.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
above_left_of   :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query (Anchor u)
above_left_of   = centerRelative ((-1),1)

-- | Value is 1 snap move down, 1 snap move left.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
below_left_of   :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query (Anchor u)
below_left_of   = centerRelative ((-1),(-1))
 

