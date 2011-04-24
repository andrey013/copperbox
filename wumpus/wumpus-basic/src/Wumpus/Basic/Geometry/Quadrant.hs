{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Geometry.Quadrant
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

module Wumpus.Basic.Geometry.Quadrant
  ( 
    Quadrant(..)

  , quadrant

  , RadialIntersect
  , QuadrantAlg(..)
  , runQuadrantAlg


  , hypotenuseQI
  , rectangleQI
  , hquadrilAcuteQI
  , hquadrilObtusQI

  , rectangleQuadrantAlg
  , diamondQuadrantAlg
  , isoscelesTriQuadrantAlg

  -- OLD...
  , rectRadialVector
  , diamondRadialVector
  , triangleRadialVector
  , triangleQI
  , rightTrapezoidQI
  
  , rightTrapeziumBaseWidth

  ) 
  where

import Wumpus.Basic.Geometry.Base

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


-- | 'reflectionModuloQI' : @ ang -> Radian @
-- 
-- Modulo an angle so it lies in quadrant I (north east) 
-- /by reflection/ - thats to say:
-- 
-- > If the angle is in QI the result is identity.
--
-- > If the angle is in QII it is reflected about the Y-axis.
-- >
-- > e.g. 170deg becomes 10deg.
-- 
-- > If the angle is in QIII it is reflected about both axes.
-- >
-- > e.g. 190deg becomes 10deg.
-- 
-- > If the angle is in QIV it is reflected about the X-axis.
-- >
-- > e.g. 350deg becomes 10deg.
-- 
--
reflectionModuloQI :: Radian -> Radian
reflectionModuloQI = step . circularModulo 
  where
    step ang | ang < 0.5*pi   = ang
             | ang < pi       = pi - ang
             | ang < 1.5*pi   = ang - pi
             | otherwise      = two_pi - ang


type RadialIntersect u = Radian -> Vec2 u


-- | A /Quadrant algorithm/.
--
data QuadrantAlg u = QuadrantAlg 
      { calc_quad1 :: RadialIntersect u
      , calc_quad2 :: RadialIntersect u
      , calc_quad3 :: RadialIntersect u
      , calc_quad4 :: RadialIntersect u
      }

runQuadrantAlg :: Radian -> QuadrantAlg u -> Vec2 u
runQuadrantAlg a qa = step (circularModulo a)
  where
    step ang | ang < half_pi  = calc_quad1 qa ang
             | ang < pi       = calc_quad2 qa ang
             | ang < 1.5*pi   = calc_quad3 qa ang
             | otherwise      = calc_quad4 qa ang

    

-- | Reuse a QI algorithm to work in QII /provided/ it works
-- under reflection.
--
reflectCalcQ2ToQ1 :: Num u => RadialIntersect u -> RadialIntersect u
reflectCalcQ2ToQ1 q1Fun = negateX . q1Fun . reflectionModuloQI


-- | Reuse a QI algorithm to work in QIII /provided/ it works
-- under reflection.
--
reflectCalcQ3ToQ1 :: Num u => RadialIntersect u -> RadialIntersect u
reflectCalcQ3ToQ1 q1Fun = negateXY . q1Fun . reflectionModuloQI


-- | Reuse a QI algorithm to work in QIV /provided/ it works
-- under reflection.
--
reflectCalcQ4ToQ1 :: Num u => RadialIntersect u -> RadialIntersect u
reflectCalcQ4ToQ1 q1Fun = negateY . q1Fun . reflectionModuloQI



--
-- Negating vectors - aka relfecting them:

-- | Negate a vector in X - aka reflect it about the Y-axis.
--
negateX :: Num u => Vec2 u -> Vec2 u
negateX (V2 x y) = V2 (-x) y

-- | Negate a vector in Y - aka reflect it about the X-axis.
--
negateY :: Num u => Vec2 u -> Vec2 u
negateY (V2 x y) = V2 x (-y)

-- | Negate a vector in X and Y - aka reflect it about both axes.
--
negateXY :: Num u => Vec2 u -> Vec2 u
negateXY (V2 x y) = V2 (-x) (-y)



-- | Builder for the usual case of /Quadrant algorithm/ where
-- each quadrant is calculated in QI then the answer is reflected 
-- to the respective quadrant.
--
-- Calulating for QI is usually easier...
--
makeReflectionQuadrantAlg :: Num u 
                          => RadialIntersect u 
                          -> RadialIntersect u
                          -> RadialIntersect u 
                          -> RadialIntersect u
                          -> QuadrantAlg u
makeReflectionQuadrantAlg f1 f2 f3 f4 = 
    QuadrantAlg { calc_quad1 = f1
                , calc_quad2 = reflectCalcQ2ToQ1 f2
                , calc_quad3 = reflectCalcQ3ToQ1 f3
                , calc_quad4 = reflectCalcQ4ToQ1 f4
                }

--------------------------------------------------------------------------------



-- | 'triangleQI' : @ dx * dy -> RadialIntersect @
--
-- Find where a line from (0,0) with elevation @ang@ intersects 
-- the hypotenuse a right triangle in QI (the legs of the triangle 
-- take the x and y-axes).  
--
-- > ang must be in the @range 0 < ang <= 90@.
-- >
-- > width and height must be positive.
--
hypotenuseQI :: (Real u, Floating u) => u -> u -> RadialIntersect u
hypotenuseQI dx dy ang = avec ang dist
  where
    base_ang = atan (dy / dx)
    apex     = pi - (base_ang + fromRadian ang)
    dist     = sin base_ang * (dx / sin apex)



-- | 'rectangleQI' : @ width * height * ang -> Vec @
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
rectangleQI dx dy ang
    | ang < theta  = let y1 = dx * fromRadian (tan ang) in V2 dx y1
    | otherwise    = let x1 = dy / fromRadian (tan ang) in V2 x1 dy
  where
    theta               = toRadian $ atan (dy/dx)



-- | 'hquadrilAcuteQI' : @ dx * dy * ang -> RadialIntersect @
--
-- Find where a line from (0,0) with elevation @ang@ intersects 
-- a quadrilateral in /H acute/ form in QI.  
--
-- > ang must be in the @range 0 < ang <= 90@.
-- >
-- > dx (top width @bc@) and dy (height @ab) must be positive.
--
-- Horizontal acute quadrilateral (@H@ because one of the two 
-- \"sides of interest\" is horizontal, /acute/ because the 
-- angle of interest @bcd@ is acute:
--
-- >      
-- >  b---*----c
-- >  |       /
-- >  |      %
-- >  |     /
-- >  a----d
-- >
--
hquadrilAcuteQI :: (Real u, Floating u) 
                => u -> u -> Radian -> RadialIntersect u
hquadrilAcuteQI bc ab bcd ang = 
    if ang >= cad then bisectingHTop ab ang else bisecting_dc ad adc ang
  where
    cad           = half_pi - (atan $ toRadian $ bc / ab)
    adc           = pi - bcd
    star_c        = ab / (fromRadian $ tan bcd)
    ad            = bc - star_c

    
-- This is intersecting dc at percent-sign - now called z.
--
-- Know one side (ad) and two angs (zad which is ang) and (adc == adz)
-- Use law of sines to find (az) :
-- 
-- >
-- >         z
-- >    . ' /
-- >  a----d
-- > 
--
bisecting_dc :: Floating u => u -> Radian -> Radian -> Vec2 u
bisecting_dc ad adc ang = avec ang az
  where
    adz  = adc
    azd  = pi - (ang + adz)
    sine = fromRadian . sin
    az   = (ad * (sine adz)) / sine azd 
  


-- This is intersecting bc at star now called o.
--
-- >  b---o----c
-- >  |  / 
-- >  | /  
-- >  |/    
-- >  a-----
--
bisectingHTop :: Fractional u => u -> Radian -> Vec2 u
bisectingHTop ab ang = V2 bo ab
  where
    bao = half_pi - ang 
    bo  = ab * (fromRadian $ tan bao)
     

-- | 'hquadrilObtusQI' : @ dx * dy * ang -> RadialIntersect @
--
-- Find where a line from (0,0) with elevation @ang@ intersects 
-- a quadrilateral in /H obtus/ form in QI.  
--
-- > ang must be in the @range 0 < ang <= 90@.
-- >
-- > dx (top width @bc@) and dy (height @ab) must be positive.
--
-- H Obtus quadrilateral (@H@ because one of the two 
-- \"sides of interest\" is horizontal, /obtus/ because the 
-- angle interest @bcd@ is obtuse:
--
-- >      
-- >  b---*----c
-- >  |         \
-- >  |          %
-- >  |           \
-- >  a------------d
-- >
--
hquadrilObtusQI :: (Real u, Floating u) 
                => u -> u -> Radian -> RadialIntersect u
hquadrilObtusQI bc ab bcd ang = 
    if ang < cad then bisecting_dc ad adc ang else bisectingHTop ab ang 
  where
    cad           = half_pi - (atan $ toRadian $ bc / ab)
    adc           = pi - bcd
    star_c        = ab / (fromRadian $ tan bcd)
    ad            = bc - star_c





     




-- | 'diamondQuadrantAlg' : @ width * height -> QuadrantAlg @
--
-- Find where a radial line extended from (0,0) with the elevation
-- @ang@ intersects with an enclosing diamond. The diamond is 
-- centered at (0,0).
-- 
-- Internally the calculation is made in quadrant I (north east),
-- symmetry is used to translate result to the other quadrants.
--
diamondQuadrantAlg :: (Real u, Floating u) => u -> u -> QuadrantAlg u
diamondQuadrantAlg w h = makeReflectionQuadrantAlg q1 q1 q1 q1
  where
    hw = 0.5 * w
    hh = 0.5 * h
    q1 = hypotenuseQI hw hh


-- | 'rectangleQuadrantAlg' : @ width * height -> QuadrantAlg @
--
-- Find where a radial line extended from (0,0) with the elevation
-- @ang@ intersects with an enclosing rectangle. The rectangle is 
-- centered at (0,0).
-- 
-- Internally the calculation is made in quadrant I (north east),
-- symmetry is used to translate result to the other quadrants.
--
rectangleQuadrantAlg :: (Real u, Floating u) => u -> u -> QuadrantAlg u
rectangleQuadrantAlg w h = makeReflectionQuadrantAlg q1 q1 q1 q1
  where
    hw = 0.5 * w
    hh = 0.5 * h
    q1 = rectangleQI hw hh

-- | 'isoscelesTriQuadrantAlg' : @ base_width * height -> QuadrantAlg @
--
-- Find where a radial line extended from (0,0) with the elevation
-- @ang@ intersects with an enclosing isosceles triangle. 
-- 
-- Note the /center/ of the triangle (0,0) is the centroid not the
-- incenter.
-- 
-- Internally the calculation is made in quadrant I (north east),
-- symmetry is used to translate result to the other quadrants.
--
isoscelesTriQuadrantAlg :: (Real u, Floating u) => u -> u -> QuadrantAlg u
isoscelesTriQuadrantAlg bw h = 
    makeReflectionQuadrantAlg qtop qtop qbase qbase
  where
    ymaj      = 2 * (h / 3)
    ymin      = h / 3
    hbw       = 0.5 * bw
    half_apex = atan (toRadian $ hbw / h)
    ctrdw     = ymaj * (fromRadian $ tan half_apex)
    ang       = half_pi - half_apex

    qtop      = hypotenuseQI ctrdw ymaj
    qbase     = hquadrilAcuteQI hbw ymin ang
    


-- OLD ...

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


-- | 'triangleRadialVector' : @ half_base_width * height_minor * 
--        height_minor * ang -> Vec @
--
-- Find where a radial line extended from (0,0) with the elevation
-- @ang@ intersects with an enclosing triangle. The triangle has 
-- the centroid at (0,0), so solutions in quadrants I and II are 
-- intersections with a simple line. Intersections in quadrants
-- III and IV can intersect either the respective side or the 
-- base.
-- 
--
triangleRadialVector :: (Real u, Floating u) => u -> u -> u -> Radian -> Vec2 u
triangleRadialVector hbw hminor hmajor ang = fn $ circularModulo ang
  where
    fn a | a < 0.5*pi   = triangleQI major_width hmajor a
         | a < pi       = negateX  $ triangleQI major_width hmajor (pi - a)
         | a < 1.5*pi   = negateXY $ rightTrapezoidQI hbw hminor base_rang (a - pi) 
         | otherwise    = negateY  $ rightTrapezoidQI hbw hminor base_rang (2*pi - a)

    height              = hmajor + hminor
    base_rang           = toRadian $ atan (height / hbw)
    major_width         = hmajor / (fromRadian $ tan base_rang)


-- | 'triangleQI' : @ width * height * ang -> Vec @
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



-- | 'rightTrapezoidQI' : @ top_width * height * top_right_ang -> Vec @
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
-- > top_width and height must be positive.
--
rightTrapezoidQI :: (Real u, Floating u) 
                 => u -> u -> Radian -> Radian -> Vec2 u
rightTrapezoidQI tw h top_rang ang = 
    if w0 <= tw then dv else avec ang minor_dist
  where
    -- dist is hypotenuse of a right triangle1
    dist         = h / (fromRadian $ sin ang)
    
    -- potentially this vector is *too long*.
    dv@(V2 w0 _) = avec ang dist

    -- this is dist *cut short* because it intersects the right
    -- side rather than the top
    minor_dist   = triangleLeftSide base_width ang lr_ang
    
    lr_ang       = pi - top_rang

    base_width   = rightTrapeziumBaseWidth tw h top_rang


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
    (fromRadian $ sin rang) / factor 
  where
    apex   = pi - (lang + rang)
    factor = (fromRadian $ sin apex) / base_width


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
    shorten  = h / fromRadian (tan tr_ang)
    extend   = let lr_ang = pi - tr_ang in h / fromRadian (tan lr_ang)