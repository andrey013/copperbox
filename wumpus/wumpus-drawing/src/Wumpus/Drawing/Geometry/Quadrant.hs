{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Geometry.Quadrant
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Quadrants and trigonometric calculations.
-- 
-- \*\* - WARNING \*\* - half baked. 
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Geometry.Quadrant
  ( 
    Quadrant(..)

  , quadrant
  , quadModulo

  , rectRadialVector
  , diamondRadialVector
  ) 
  where

import Wumpus.Core                              -- package: wumpus-core

data Quadrant = QUAD_NE | QUAD_NW | QUAD_SW | QUAD_SE
  deriving (Enum,Eq,Ord,Show)

-- | get the quadrant of an angle
quadrant :: Radian -> Quadrant
quadrant = fn . circularModulo
  where
    fn a | a < 0.5*pi   = QUAD_NE
         | a < pi       = QUAD_NW
         | a < 1.5*pi   = QUAD_SW
         | otherwise    = QUAD_SE


-- | Modulo a angle into the range @0..(pi/2)@.
--
quadModulo :: Radian -> Radian 
quadModulo r = d2r $ dec + (fromIntegral $ i `mod` 90)
  where
    i       :: Integer
    dec     :: Double
    (i,dec) = properFraction $ r2d r


--------------------------------------------------------------------------------

negateX :: Num u => Vec2 u -> Vec2 u
negateX (V2 x y) = V2 (-x) y

negateY :: Num u => Vec2 u -> Vec2 u
negateY (V2 x y) = V2 x (-y)

negateXY :: Num u => Vec2 u -> Vec2 u
negateXY (V2 x y) = V2 (-x) (-y)


-- | Perform the calculation only in QI, use symmetry to translate
-- the result to the other quadrants.
--
rectRadialVector :: (Real u, Floating u) => u -> u -> Radian -> Vec2 u
rectRadialVector hw hh ang = fn $ circularModulo ang
  where
    theta               = toRadian $ atan (hh/hw)
    fn a | a < 0.5*pi   = qi a
         | a < pi       = negateX  $ qi (pi - a)
         | a < 1.5*pi   = negateXY $ qi (a - pi) 
         | otherwise    = negateY  $ qi (2*pi - a)
    
    qi a | a < theta    = let y = hw * fromRadian (tan a) in V2 hw y
         | otherwise    = let x = hh / fromRadian (tan a) in V2 x hh


diamondRadialVector :: (Real u, Floating u) => u -> u -> Radian -> Vec2 u
diamondRadialVector hw hh ang = fn $ circularModulo ang
  where
    fn a | a < 0.5*pi   = diamondQI hw hh a
         | a < pi       = negateX  $ diamondQI hw hh (pi - a)
         | a < 1.5*pi   = negateXY $ diamondQI hw hh (a - pi) 
         | otherwise    = negateY  $ diamondQI hw hh (2*pi - a)


-- | Find the base angle, then use the law of sines.
--
diamondQI :: (Real u, Floating u) => u -> u -> Radian -> Vec2 u
diamondQI hw hh ang = avec ang dist
  where
    base_ang = atan (hh / hw)
    apex     = pi - (base_ang + fromRadian ang)
    dist     = sin base_ang * (hw / sin apex)

