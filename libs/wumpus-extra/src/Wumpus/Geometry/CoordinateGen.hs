{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.Base
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Generating sequences of coordinates (points).
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.CoordinateGen
  (
    circular
  , circularAbout

  , ixDownLeftRight
  , ixLeftRightDown
  , ixLeftRightUp
  , ixUpLeftRight

  , calendarGrid

  -- * Helpers
  , countup
  , countdown

  ) where

import Wumpus.Core

import Data.AffineSpace

import Data.List


--------------------------------------------------------------------------------
-- Generating points on a circle

-- | @ circle n r @ 
-- Generate @n@ equally spaced points around a circle of radius @r@
-- centered at the origin. The points proceed counter-clockwise 
-- from the the initial point on the x-axis.
circular :: (Floating u , Real u) => Int -> u -> [Point2 u]
circular = circularAbout zeroPt


-- | @ circularAbout pt n r ...@
-- Trace @n@ equally spaced points around a circle of radius @r@
-- centered at @pt@. The points proceed counter-clockwise 
-- from the the initial point on the x-axis.
--
circularAbout :: (Floating u , Real u) 
              => Point2 u -> Int -> u -> [Point2 u]
circularAbout pt n r = take n $ iterate (rotateAbout ang pt) px
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

