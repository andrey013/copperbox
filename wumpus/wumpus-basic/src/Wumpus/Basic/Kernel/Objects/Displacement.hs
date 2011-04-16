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
  , displaceVec
  , displaceH
  , displaceV

  , disp_up
  , disp_down
  , disp_left
  , disp_right

  , disp_up_left
  , disp_up_right
  , disp_down_left
  , disp_down_right
  

  , disp_north
  , disp_south
  , disp_east
  , disp_west

  , disp_northeast
  , disp_northwest
  , disp_southeast
  , disp_southwest


  , displaceParallel
  , displacePerpendicular
  , displaceOrtho

  , adisp_north
  , adisp_south
  , adisp_east
  , adisp_west  

  , adisp_northeast
  , adisp_northwest
  , adisp_southeast
  , adisp_southwest

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




disp_up :: Num u => u -> PointDisplace u
disp_up = displaceV

disp_down :: Num u => u -> PointDisplace u
disp_down = displaceV . negate

disp_left :: Num u => u -> PointDisplace u
disp_left = displaceH . negate

disp_right :: Num u => u -> PointDisplace u
disp_right = displaceH


-- diagonals - these are different to cardinals which have the
-- hypotenuese as the dist.
--

disp_up_left :: Num u => u -> PointDisplace u
disp_up_left u = displaceVec (V2 (-u) u)

disp_up_right :: Num u => u -> PointDisplace u
disp_up_right u = displaceVec (V2 u u)

disp_down_left :: Num u => u -> PointDisplace u
disp_down_left u = displaceVec (V2 (-u) (-u))

disp_down_right :: Num u => u -> PointDisplace u
disp_down_right u = displaceVec (V2 u (-u))







-- Cardinal displacement 

disp_north :: Num u => u -> PointDisplace u
disp_north = displaceV


disp_south :: Num u => u -> PointDisplace u
disp_south =  displaceV . negate

disp_east :: Num u => u -> PointDisplace u
disp_east = displaceH

disp_west :: Num u => u -> PointDisplace u
disp_west = displaceH . negate

disp_northeast :: Floating u => u -> PointDisplace u
disp_northeast = displaceVec . avec (0.25 * pi)

disp_northwest ::  Floating u => u -> PointDisplace u
disp_northwest = displaceVec . avec (0.75 * pi)

disp_southeast ::  Floating u => u -> PointDisplace u
disp_southeast = displaceVec . avec (1.75 * pi)

disp_southwest ::  Floating u => u -> PointDisplace u
disp_southwest = displaceVec . avec (1.25 * pi)


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


adisp_north :: Floating u => u -> ThetaPointDisplace u
adisp_north = displacePerpendicular


adisp_south :: Floating u => u -> ThetaPointDisplace u
adisp_south = displacePerpendicular . negate


adisp_east :: Floating u => u -> ThetaPointDisplace u
adisp_east = displaceParallel


adisp_west :: Floating u => u -> ThetaPointDisplace u
adisp_west = displaceParallel . negate

adisp_northeast :: Floating u => u -> ThetaPointDisplace u
adisp_northeast d = \ang pt -> pt .+^ avec (ang + (0.25*pi)) d


adisp_northwest :: Floating u => u -> ThetaPointDisplace u
adisp_northwest d = \ang pt -> pt .+^ avec (ang + (0.75*pi)) d


adisp_southeast :: Floating u => u -> ThetaPointDisplace u
adisp_southeast d = \ang pt -> pt .+^ avec (ang + (1.75*pi)) d


adisp_southwest :: Floating u => u -> ThetaPointDisplace u
adisp_southwest d = \ang pt -> pt .+^ avec (ang + (1.25*pi)) d



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
 

