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
-- \*\* - WARNING \*\* - in progress. 
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Geometry.Quadrant
  ( 
    Quadrant(..)

  , quadrant
  , qiModulo

  , rectRadialVector
  , rectangleQI

  , diamondRadialVector
  , triangleRadialVector
  , triangleQI
  , rightTrapezoidQI
  
  , rightTrapeziumBaseWidth

  ) 
  where

import Wumpus.Core                              -- package: wumpus-core

data Quadrant = QUAD_NE | QUAD_NW | QUAD_SW | QUAD_SE
  deriving (Enum,Eq,Ord,Show)

-- | 'quadrant' : @ ang -> Quadrant @
--
-- Get the quadrant of an angle.
--
quadrant :: Radian -> Quadrant
quadrant = fn . circularModulo
  where
    fn a | a < 0.5*pi   = QUAD_NE
         | a < pi       = QUAD_NW
         | a < 1.5*pi   = QUAD_SW
         | otherwise    = QUAD_SE


-- | 'qiModulo' : @ ang -> Radian @
-- 
-- Modulo an angle so it lies in quadrant I (north east), 
-- i.e. modulo into the range @0..(pi/2)@.
--
qiModulo :: Radian -> Radian 
qiModulo r = d2r $ dec + (fromIntegral $ i `mod` 90)
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


-- | 'rectRadialVector' : @ half_width * half_height * ang -> Vec @
--
-- Find where a radial line extended from (0,0) with the elevation
-- @ang@ intersects with an enclosing rectangle. The rectangle is 
-- centered at (0,0).
-- 
-- Internally the calculation is made in quadrant I (north east),
-- symmetry is used to translate result to the other quadrants.
--
rectRadialVector :: (Real u, Floating u) => u -> u -> Radian -> Vec2 u
rectRadialVector hw hh ang = fn $ circularModulo ang
  where
    fn a | a < 0.5*pi   = rectangleQI hw hh a
         | a < pi       = negateX  $ rectangleQI hw hh (pi - a)
         | a < 1.5*pi   = negateXY $ rectangleQI hw hh (a - pi) 
         | otherwise    = negateY  $ rectangleQI hw hh (2*pi - a)


-- | 'rectangleQI' : @ width * height * ang -> Vec2 @
--
-- Find where a line from (0,0) in direction @ang@ intersects the 
-- top or right side of a rectangle in QI (left side is the 
-- y-axis, bottom is the x-axis).  
--
-- > ang must be in the @range 0 < ang <= 90 deg@.
-- >
-- > width and height must be positive.
--
rectangleQI :: (Real u, Floating u) => u -> u -> Radian -> Vec2 u    
rectangleQI w h ang
    | ang < theta  = let y = w * fromRadian (tan ang) in V2 h y
    | otherwise    = let x = h / fromRadian (tan ang) in V2 x h
  where
    theta               = toRadian $ atan (h/w)


-- | 'diamondRadialVector' : @ half_width * half_height * ang -> Vec @
--
-- Find where a radial line extended from (0,0) with the elevation
-- @ang@ intersects with an enclosing diamond. The diamond is 
-- centered at (0,0).
-- 
-- Internally the calculation is made in quadrant I (north east),
-- symmetry is used to translate result to the other quadrants.
--
diamondRadialVector :: (Real u, Floating u) => u -> u -> Radian -> Vec2 u
diamondRadialVector hw hh ang = fn $ circularModulo ang
  where
    fn a | a < 0.5*pi   = triangleQI hw hh a
         | a < pi       = negateX  $ triangleQI hw hh (pi - a)
         | a < 1.5*pi   = negateXY $ triangleQI hw hh (a - pi) 
         | otherwise    = negateY  $ triangleQI hw hh (2*pi - a)


triangleRadialVector :: (Real u, Floating u) => u -> u -> u -> Radian -> Vec2 u
triangleRadialVector hbw hminor hmajor ang = fn $ circularModulo ang
  where
    fn a | a < 0.5*pi   = triangleQI major_width hmajor a
         | a < pi       = negateX  $ triangleQI major_width hmajor (pi - a)
         | a < 1.5*pi   = negateXY $ rightTrapezoidQI hbw hminor base_rang (a - pi) 
         | otherwise    = negateY  $ rightTrapezoidQI hbw hminor base_rang (2*pi - a)

    height              = hmajor + hminor
    base_rang           = toRadian $ atan (hbw / height)
    major_width         = hmajor * (fromRadian $ tan base_rang)


-- | 'triangleQI' : @ width * height * ang -> Vec2 @
--
-- Find where a line from (0,0) with elevation @ang@ intersects 
-- the hypotenuse a right triangle in QI (the legs of the triangle 
-- take the x and y-axes).  
--
-- > ang must be in the @range 0 < ang <= 90@.
-- >
-- > width and height must be positive.
--
triangleQI :: (Real u, Floating u) => u -> u -> Radian -> Vec2 u
triangleQI w h ang = avec ang dist
  where
    base_ang = atan (h / w)
    apex     = pi - (base_ang + fromRadian ang)
    dist     = sin base_ang * (w / sin apex)



-- | 'rightTrapezoidQI' : @ top_width * height * top_right_ang -> Vec2 @
--
-- Find where a line from (0,0) with elevation @ang@ intersects 
-- the either the lines A_B or B_D in a right trapezoid in QI. 
-- 
-- The right trapezoid has a variable right side. Left side is the
-- y-axis (C_A), bottom side is the x-axis (C_D), top side is 
-- parallel to the x-axis (A_B).
--
-- >  A   B
-- >  -----
-- >  |    \
-- >  |     \     
-- >  -------
-- >  C      D
--
-- >  A      B
-- >  -------
-- >  |     /
-- >  |    /     
-- >  -----
-- >  C   D
--
-- > ang must be in the range 0 < ang <= 90.
-- >
-- > width and height must be positive.
--
rightTrapezoidQI :: (Real u, Floating u) 
                 => u -> u -> Radian -> Radian -> Vec2 u
rightTrapezoidQI w h top_rang ang = 
    if w0 <= w then dv
               else avec ang minor_dist
  where
    -- dist is hypotenuse of a right triangle1
    dist         = w / (fromRadian $ sin ang)
    
    -- potentially this vector is *too long*.
    dv@(V2 w0 _) = avec ang dist

    -- this is dist *cut short* because it intersects the right
    -- side rather than the top
    minor_dist   = triangleLeftSide base_width ang lr_ang
    
    lr_ang       = pi - top_rang

    base_width   = rightTrapeziumBaseWidth w h top_rang



-- Legend:
-- 
-- > @top_rang@ is A/B\C.
-- >
-- > @ang@ is B/C\D.
-- > 
-- > w (width) is A_B.
--
-- > h (height) is C_A.
-- 
-- >  A     B
-- >  ------ 
-- >  |    / .
-- >  |   /   .
-- >  |  /     .
-- >  | /       .     
-- >  |/..........
-- >  C          D
--
-- Synthetically:
--
-- > Right triangle 1 is ABC.
-- >
-- > @dist@ is C_B.
-- >
-- > @base_width@ is C_D.
-- >
-- > @lr_ang is C/D\B. 
--   


-- | 'traingleLeftSide' : @ base_width * left_ang * right_ang -> Length @
--
-- >  
-- >        C   
-- >       /\
-- >      /  \
-- >     /    \
-- >    /      \    
-- >   /________\
-- >  A          B
-- >
-- 
-- > Calculate A_C given side A_B, angle C/A\B and angle A/B\C.
--
triangleLeftSide :: Fractional u => u -> Radian -> Radian -> u
triangleLeftSide base_width lang rang = 
    (fromRadian $ sin rang) * (base_width / fromRadian apex)
  where
    apex = pi - (lang + rang)

-- | 'rightTrapeziumBaseWidth' : @ top_width * height * top_right_ang -> Length @
-- 
-- Find the length of the line C_D:
--
-- >  A   B
-- >  -----
-- >  |    \
-- >  |     \     
-- >  -------
-- >  C      D
--
-- >  A      B
-- >  -------
-- >  |     /
-- >  |    /     
-- >  -----
-- >  C   D
--
-- 
rightTrapeziumBaseWidth :: Fractional u => u -> u -> Radian -> u
rightTrapeziumBaseWidth tw h tr_ang 
    | tr_ang < half_pi = tw - shorten
    | tr_ang > half_pi = tw + extend
    | otherwise        = tw
  where
    half_pi  = 0.5*pi
    shorten  = h / fromRadian (tan tr_ang)
    extend   = h / fromRadian (tan $ tr_ang - half_pi)