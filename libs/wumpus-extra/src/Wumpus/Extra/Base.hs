{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Base
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Type classes and common things.
--
--------------------------------------------------------------------------------


module Wumpus.Extra.Base 
  (
  -- * Type classes
    Converse(..)
  , CCWAngle(..)
  , ExtractPath(..)
  , ObjectLength(..)
  , Midpoint(..)

  -- * Operations
  , subdivisions
  , midpointBetween
  , strline
  , strlineMidpt

  -- * Dash patterns
  , dashOffset
  , evenDashes
  , solid

  -- * Coordinate generation
  , circular
  , circularAbout

  , ixDownLeftRight
  , ixLeftRightDown
  , ixLeftRightUp
  , ixUpLeftRight

  , calendarGrid

  -- * Helpers
  , countup
  , countdown

  , rescale
  , clamp
  , inrange
  , order

  ) where

import Wumpus.Core

import Data.AffineSpace
import Data.VectorSpace

import Data.List ( unfoldr )

--------------------------------------------------------------------------------
-- Type classes

-- | Reverse the direction of some ordered object (line segment, 
-- bezier curve, ...).
--
class Converse a where 
  converse :: a -> a

-- | Counter-clockwise angle formed with the horizontal x-axis.
--
class CCWAngle a where ccwAngle :: a -> Radian


-- | Extract the path from some object that can be /traced/.
class ExtractPath a where 
  extractPath :: a -> Path (DUnit a)

-- | Length of object (e.g. LineSegment).
class ObjectLength a where 
  objectLength :: a -> (DUnit a)

class Midpoint a where
  midpoint :: a -> Point2 (DUnit a)

--------------------------------------------------------------------------------
-- Instances for Wumpus core

instance Converse Radian where
  converse = norm . (+pi)
    where norm = id -- to do
         



--------------------------------------------------------------------------------
-- Common ...


-- Subdivide a positive non-zero fractional number n times 
-- returning the list @[0, a/n, 2*a/n, ..., a]@  
subdivisions :: Fractional a => Int -> a -> [a]
subdivisions i a = take (i+1) $ iterate (+n) 0 where 
   n = a / fromIntegral i


-- | Midpoint between two points.
midpointBetween :: Fractional u => Point2 u -> Point2 u -> Point2 u
midpointBetween p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0



strline :: Num u => Point2 u -> Vec2 u -> u -> Point2 u
strline pt u t = pt .+^ (t *^ u)

strlineMidpt :: Floating u => Point2 u -> Vec2 u -> Point2 u
strlineMidpt pt u = strline pt u 0.5


--------------------------------------------------------------------------------


-- Should these produce a DashPattern or a StrokeAttr?

evenDashes :: Int -> DashPattern 
evenDashes n = Dash 0 [(n,n)]

dashOffset :: Int -> DashPattern -> DashPattern
dashOffset _ Solid       = Solid
dashOffset n (Dash _ xs) = Dash n xs


-- solid creating a StrokeAttr rather than a DashPattern seems 
-- uncontroversial ...
solid :: StrokeAttr
solid = DashPattern Solid


--------------------------------------------------------------------------------
-- Generating points on a circle

-- | @ circle n r @ 
-- Generate @n@ equally spaced points around a circle of radius @r@
-- centered at the origin. The points proceed counter-clockwise 
-- from the the initial point on the x-axis.
circular :: (Floating u , Real u) => Int -> u -> [Point2 u]
circular n r = circularAbout n r zeroPt


-- | @ circularAbout pt n r ...@
-- Trace @n@ equally spaced points around a circle of radius @r@
-- centered at @pt@. The points proceed counter-clockwise 
-- from the the initial point on the x-axis.
--
circularAbout :: (Floating u , Real u) 
              => Int -> u -> Point2 u -> [Point2 u]
circularAbout n r pt = take n $ iterate (rotateAbout ang pt) px
  where
    ang  = let n'::Double = fromIntegral n in toRadian $  2*pi/ n'
    px   = pt .+^ V2 0 r

-- | Generate points in a grid - move down a whole column, move 
-- right one, move down the next column.
-- 
-- Points are generated from count-1 to 0, but can be scaled 
-- or have the offest shifted with the point transformer function.
--
ixDownLeftRight :: (Num u)
                => Int -> Int -> (Point2 u -> Point2 u) -> [Point2 u]
ixDownLeftRight row_count col_count fn = 
    [fn $ P2 x y | x <- countup   (row_count - 1)
                 , y <- countdown (col_count - 1) ]

-- | Generate points in a grid - move up a whole column, move 
-- right one, move up the next column.
--
ixUpLeftRight :: (Num u)
              => Int -> Int -> (Point2 u -> Point2 u) -> [Point2 u]
ixUpLeftRight row_count col_count fn = 
    [fn $ P2 x y | x <- countup (row_count - 1)
                 , y <- countup (col_count - 1) ]
                 


-- | Generate points in a grid - move from left to right in a 
-- row, move down one, move left to right through the next row.
-- 
ixLeftRightDown :: (Num u)
                => Int -> Int -> (Point2 u -> Point2 u) -> [Point2 u]
ixLeftRightDown row_count col_count fn = 
    [fn $ P2 x y | y <- countdown (col_count - 1)
                 , x <- countup   (row_count - 1) ]


-- | Generate points in a grid - move from left to right in a 
-- row, move up one, move left to right through the next row.
-- 
ixLeftRightUp :: (Num u)
              => Int -> Int -> (Point2 u -> Point2 u) -> [Point2 u]
ixLeftRightUp row_count col_count fn = 
    [fn $ P2 x y | y <- countup (col_count - 1)
                 , x <- countup (row_count - 1) ]




calendarGrid :: (Num u)
             => Int -> Int -> (Point2 u -> Point2 u) -> [Point2 u]
calendarGrid start count = take count . drop start . ixLeftRightDown 6 7 

--------------------------------------------------------------------------------
-- Helpers (not necessarily private)

-- | Countdown from n to 0.
countdown :: Num u => Int -> [u]
countdown = unfoldr phi where
   phi i | i < 0 = Nothing
   phi i         = Just (fromIntegral i,i-1)

-- | Count up to n from 0. 
countup :: Num u => Int -> [u]
countup n = unfoldr phi 0 where
   phi i | i > n = Nothing
   phi i         = Just (fromIntegral i,i+1)



--------------------------------------------------------------------------------

rescale :: Fractional a => (a,a) -> (a,a) -> a -> a
rescale (amin,amax) (bmin,bmax) a = 
    bmin + apos * (brange / arange)  
  where
    arange = amax - amin
    brange = bmax - bmin
    apos   = a - amin 

-- | @clamp min max a@ - clamp a to be with in the bounds 
-- min..max
--
clamp :: Ord a => (a,a) -> a -> a
clamp (amin,amax) a = max amin (min amax a)

inrange :: Ord a => a -> (a,a) -> Bool
a `inrange` (amin,amax) = a >= amin && a <= amax

order :: Ord a => a -> a -> (a,a)
order a b = (min a b, max a b)