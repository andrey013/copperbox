{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Trial
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- /Trails/ - prototype paths. Less resource heavy than the Path
-- object in Wumpus-Drawing.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Trail
  (

    TrailSegment(..)
  , CatTrail
  , AnaTrail

  , drawAnaTrail
  , drawCatTrail

  , destrAnaTrail
  , destrCatTrail

  , anaCatTrail
  
  , trailIterateLocus

  , anaTrailPoints

  , rectangleTrail
  , diamondTrail
  , polygonTrail


  , catline
  , catcurve
  
  , trail_up
  , trail_down
  , trail_left
  , trail_right

  , trail_north
  , trail_south
  , trail_east
  , trail_west
  , trail_north_east
  , trail_north_west
  , trail_south_east
  , trail_south_west

  , trail_up_left
  , trail_up_right
  , trail_down_left
  , trail_down_right

  , orthoCatline

  , trail_theta_up
  , trail_theta_down
  , trail_theta_left
  , trail_theta_right

  , trail_theta_north
  , trail_theta_south
  , trail_theta_east
  , trail_theta_west
  , trail_theta_north_east
  , trail_theta_north_west
  , trail_theta_south_east
  , trail_theta_south_west

  , trail_theta_up_left
  , trail_theta_up_right
  , trail_theta_down_left
  , trail_theta_down_right


 
  , semicircleCW
  , semicircleCCW

  , minorCircleSweepCW
  , minorCircleSweepCCW
  , circleSweepCW
  , circleSweepCCW
  , circularArcCW
  , circularArcCCW

  , sineWave
  , sineWave1
  , squareWave
  , sawtoothWave
  , squiggleWave
  , semicircleWave

  , tricurve
  , rectcurve
  , trapcurve
  , bowcurve
  , wedgecurve
  , loopcurve

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.List ( unfoldr )
import Data.Monoid

-- | Trail with an initial (undrawn) movement - an anacrusis.
--
-- This allows trails to represent centered objects.
--
data AnaTrail u = AnaTrail
      { pt_init_vec :: Vec2 u
      , pt_segments :: [TrailSegment u]
      }
  deriving (Eq,Ord,Show)

type instance DUnit (AnaTrail u) = u

-- | Trail supporting concatenation.
--
newtype CatTrail u = CatTrail { getCatTrail :: H (TrailSegment u) }

type instance DUnit (CatTrail u) = u


-- | Trail segment - trails are /prototype/ paths, so the are 
-- built from the usual straight lines and Bezier curves.
--
data TrailSegment u = TLine (Vec2 u)
                    | TCurve (Vec2 u) (Vec2 u) (Vec2 u)
  deriving (Eq,Ord,Show)

type instance DUnit (TrailSegment u) = u


instance Functor TrailSegment where
  fmap f (TLine v1)        = TLine $ fmap f v1
  fmap f (TCurve v1 v2 v3) = TCurve (fmap f v1) (fmap f v2) (fmap f v3)


instance Monoid (CatTrail u) where
  mempty        = CatTrail emptyH
  a `mappend` b = CatTrail $ getCatTrail a `appendH` getCatTrail b


--------------------------------------------------------------------------------


drawCatTrail :: InterpretUnit u => PathMode -> CatTrail u -> LocGraphic u
drawCatTrail mode (CatTrail ct) = promoteLoc $ \pt -> 
    drawTrailBody mode (toListH ct) pt 


drawAnaTrail :: InterpretUnit u => PathMode -> AnaTrail u -> LocGraphic u
drawAnaTrail mode (AnaTrail v0 xs) = promoteLoc $ \pt -> 
    drawTrailBody mode xs (pt .+^ v0)


-- | Note - this optimizes contiguous lines that share the same 
-- direction. 
--
drawTrailBody :: InterpretUnit u 
              => PathMode -> [TrailSegment u] -> Point2 u -> Graphic u
drawTrailBody mode ts pt = 
    normalizeCtxF pt >>= \dpt -> 
    mapM normalizeCtxF ts >>= \dxs ->
    dcPath mode $ relPrimPath dpt $ stepA id dxs
  where
    stepA f []                   = toListH f
    stepA f (TLine v1:ys)        = stepB f (vdirection v1) v1 ys
    stepA f (TCurve v1 v2 v3:ys) = stepA (f `snocH` relCurveTo v1 v2 v3) ys

    stepB f dir v0 (TLine v1:ys) 
        | vdirection v1 == dir   = stepB f dir (v0 ^+^ v1) ys
    stepB f _   v0 ys            = stepA (f `snocH` relLineTo v0) ys


-- | /Destructor/ for the opaque 'AnaTrail' type.
--
destrAnaTrail :: AnaTrail u -> (Vec2 u, [TrailSegment u])
destrAnaTrail (AnaTrail v0 ss) = (v0,ss)

-- | /Destructor/ for the opaque 'CatTrail' type.
--
destrCatTrail :: CatTrail u -> [TrailSegment u]
destrCatTrail = toListH . getCatTrail



-- | Turn a 'CatTrail' into a 'AnaTrail'.
--
anaCatTrail :: Vec2 u -> CatTrail u -> AnaTrail u
anaCatTrail vinit cat = AnaTrail { pt_init_vec = vinit
                                      , pt_segments = getCatTrail cat []
                                      }



-- | Create a AnaTrail from the vector list - each vector in the 
-- input list iterates to the start point rather then the 
-- cumulative tip.
--
-- When the AnaTrail is run, the supplied point is the /locus/ of 
-- the path and it does not form part of the path proper.
-- 
-- Like 'trailStartIsLocus', this constructor is typically used to 
-- make /shape paths/. Some shapes are easier to express as 
-- iterated displacements of the center rather than 
-- /turtle drawing/. 
-- 
trailIterateLocus :: Num u => [Vec2 u] -> AnaTrail u
trailIterateLocus []      = AnaTrail zeroVec []
trailIterateLocus (v0:xs) = AnaTrail v0 (step v0 xs)
  where
    step v1 []      = [ TLine (v0 ^-^ v1) ]
    step v1 (v2:vs) = TLine (v2 ^-^ v1) : step v2 vs


anaTrailPoints :: InterpretUnit u => AnaTrail u -> LocQuery u [Point2 u]
anaTrailPoints (AnaTrail v0 ts) = qpromoteLoc $ \pt -> 
    return $ step (pt .+^ v0) ts
  where
    step p1 []                    = [p1]
    step p1 (TLine v1:xs)         = p1 : step (p1 .+^ v1) xs
    step p1 (TCurve v1 v2 v3 :xs) = let p2 = p1 .+^ v1
                                        p3 = p2 .+^ v2
                                        p4 = p3 .+^ v3 
                                    in p1 : p2 : p3 : step p4 xs

-- | 'rectangleTrail' : @ width * height -> AnaTrail @
--
rectangleTrail :: Fractional u => u -> u -> AnaTrail u
rectangleTrail w h = 
    AnaTrail { pt_init_vec = ctr_to_bl 
                , pt_segments = map TLine spec
                }
  where
    ctr_to_bl = vec (negate $ 0.5*w) (negate $ 0.5*h)
    spec      = [ go_right w, go_up h, go_left w, go_down h ]




-- | 'diamondTrail' : @ half_width * half_height -> AnaTrail @
--
diamondTrail :: Num u => u -> u -> AnaTrail u
diamondTrail hw hh = trailIterateLocus [ vs,ve,vn,vw ]
  where
    vs = vvec (-hh)
    ve = hvec hw
    vn = vvec hh
    vw = hvec (-hw)


-- | 'polygonTrail' : @ num_points * radius -> AnaTrail @ 
--
polygonTrail :: Floating u => Int -> u -> AnaTrail u
polygonTrail n radius = trailIterateLocus $ unfoldr phi (0,top)
  where
    top                     = 0.5*pi
    theta                   = (2*pi) / fromIntegral n
    
    phi (i,ang) | i < n     = Just (avec ang radius, (i+1,ang+theta))
                | otherwise = Nothing




catline :: Vec2 u -> CatTrail u 
catline = CatTrail . wrapH . TLine



catcurve :: Vec2 u -> Vec2 u -> Vec2 u -> CatTrail u
catcurve v1 v2 v3 = CatTrail $ wrapH $ TCurve v1 v2 v3


trail_up :: Num u => u -> CatTrail u
trail_up = catline . go_up

trail_down :: Num u => u -> CatTrail u
trail_down = catline . go_down

trail_left :: Num u => u -> CatTrail u
trail_left = catline . go_left

trail_right :: Num u => u -> CatTrail u
trail_right = catline . go_right


trail_north :: Num u => u -> CatTrail u
trail_north = trail_up

trail_south :: Num u => u -> CatTrail u
trail_south = catline . go_down

trail_east :: Num u => u -> CatTrail u
trail_east = catline . go_right

trail_west :: Num u => u -> CatTrail u
trail_west = catline . go_left


trail_north_east :: Floating u => u -> CatTrail u
trail_north_east = catline . go_north_east

trail_north_west :: Floating u => u -> CatTrail u
trail_north_west = catline . go_north_west

trail_south_east :: Floating u => u -> CatTrail u
trail_south_east = catline . go_south_east

trail_south_west :: Floating u => u -> CatTrail u
trail_south_west = catline . go_south_west


trail_up_left :: Num u => u -> CatTrail u
trail_up_left = catline . go_up_left

trail_up_right :: Num u => u -> CatTrail u
trail_up_right = catline . go_up_right

trail_down_left :: Num u => u -> CatTrail u
trail_down_left = catline . go_down_left

trail_down_right :: Num u => u -> CatTrail u
trail_down_right = catline . go_down_right


-- | Alternative to @catline@, specifying the vector components 
-- rather the vector itself.
--
orthoCatline :: Floating u => u -> u -> Radian -> CatTrail u 
orthoCatline x y ang = catline (orthoVec x y ang)


trail_theta_up :: Floating u => u -> Radian -> CatTrail u
trail_theta_up u = catline . theta_up u

trail_theta_down :: Floating u => u -> Radian -> CatTrail u
trail_theta_down u = catline . theta_down u

trail_theta_left :: Floating u => u -> Radian -> CatTrail u
trail_theta_left u = catline . theta_left u

trail_theta_right :: Floating u => u -> Radian -> CatTrail u
trail_theta_right u = catline . theta_right u


trail_theta_north :: Floating u => u -> Radian -> CatTrail u
trail_theta_north = trail_theta_up

trail_theta_south :: Floating u => u -> Radian -> CatTrail u
trail_theta_south = trail_theta_down

trail_theta_east :: Floating u => u -> Radian -> CatTrail u
trail_theta_east = trail_theta_right

trail_theta_west :: Floating u => u -> Radian -> CatTrail u
trail_theta_west = trail_theta_left


trail_theta_north_east :: Floating u => u -> Radian -> CatTrail u
trail_theta_north_east u = catline . theta_north_east u

trail_theta_north_west :: Floating u => u -> Radian -> CatTrail u
trail_theta_north_west u = catline . theta_north_west u

trail_theta_south_east :: Floating u => u -> Radian -> CatTrail u
trail_theta_south_east u = catline . theta_south_east u

trail_theta_south_west :: Floating u => u -> Radian -> CatTrail u
trail_theta_south_west u = catline . theta_south_west u


trail_theta_up_left :: Floating u => u -> Radian -> CatTrail u
trail_theta_up_left u = catline . theta_up_left u

trail_theta_up_right :: Floating u => u -> Radian -> CatTrail u
trail_theta_up_right u = catline . theta_up_right u

trail_theta_down_left :: Floating u => u -> Radian -> CatTrail u
trail_theta_down_left u = catline . theta_down_left u

trail_theta_down_right :: Floating u => u -> Radian -> CatTrail u
trail_theta_down_right u = catline . theta_down_right u



--------------------------------------------------------------------------------

--
-- DESIGN NOTE
--
-- Angle, unit width and number of repetitions (plus height etc.) 
-- seems the best API, although this make fitting an issue.
--


sineWave :: (Real u, Floating u) => Int -> u -> Radian -> CatTrail u
sineWave i unit ang = 
    mconcat $ replicate i $ sineWave1 (0.25 * unit) unit ang


-- | One-phase sine wave. Height is parametric.
--
sineWave1 :: (Real u, Floating u)
              => u -> u -> Radian -> CatTrail u
sineWave1 h unit ang = 
              catcurve  v1            (vdiff v1 v2)   (vdiff v2 v3)
    `mappend` catcurve (vdiff v3 v4)  (vdiff v4 v5)   (vdiff v5 v6)
    `mappend` catcurve (vdiff v6 v7)  (vdiff v7 v8)   (vdiff v8 v9)
    `mappend` catcurve (vdiff v9 v10) (vdiff v10 v11) (vdiff v11 v12)
  where
    base1 = unit / 12
    h2    = h * (pi / 6)
    v1    = orthoVec     base1    h2  ang
    v2    = orthoVec  (2*base1)   h   ang
    v3    = orthoVec  (3*base1)   h   ang
    v4    = orthoVec  (4*base1)   h   ang
    v5    = orthoVec  (5*base1)   h2  ang
    v6    = orthoVec  (6*base1)   0   ang
    v7    = orthoVec  (7*base1) (-h2) ang
    v8    = orthoVec  (8*base1) (-h)  ang
    v9    = orthoVec  (9*base1) (-h)  ang
    v10   = orthoVec (10*base1) (-h)  ang
    v11   = orthoVec (11*base1) (-h2) ang
    v12   = orthoVec (12*base1)   0   ang



kappa :: Floating u => u
kappa = 4 * ((sqrt 2 - 1) / 3)


-- DESIGN NOTE - different functions for CW and CCW or same 
-- function with @ClockDirection@ as first argument?


-- | 'semicircleCW' : @ base_vector -> CatTrail @ 
-- 
-- Make a clockwise semicircle from two Bezier curves. Although 
-- this function produces an approximation of a semicircle, the 
-- approximation seems fine in practice.
--
semicircleCW :: (Real u, Floating u) => Vec2 u -> CatTrail u
semicircleCW base_vec =
              catcurve  v1           (vdiff v1 v2) (vdiff v2 v3)
    `mappend` catcurve (vdiff v3 v4) (vdiff v4 v5) (vdiff v5 v6)
  where
    circum  = vlength base_vec
    radius  = 0.5 * circum
    ang     = vdirection base_vec
    rl      = radius * kappa
    
    v1      = orthoVec 0 rl ang
    v2      = orthoVec (radius - rl) radius ang
    v3      = orthoVec radius radius ang

    v4      = orthoVec (radius + rl) radius ang
    v5      = orthoVec circum rl ang
    v6      = orthoVec circum 0 ang


-- | 'semicircleCCW' : @ base_vector_vector -> CatTrail @ 
-- 
-- Make a counter-clockwise semicircle from two Bezier curves. 
-- Although this function produces an approximation of a 
-- semicircle, the approximation seems fine in practice.
--
semicircleCCW :: (Real u, Floating u) => Vec2 u -> CatTrail u
semicircleCCW base_vec =
              catcurve  v1           (vdiff v1 v2) (vdiff v2 v3)
    `mappend` catcurve (vdiff v3 v4) (vdiff v4 v5) (vdiff v5 v6)
  where
    circum  = vlength base_vec
    radius  = 0.5 * circum
    ang     = vdirection base_vec
    rl      = radius * kappa
    
    v1      = orthoVec 0 (-rl) ang
    v2      = orthoVec (radius - rl) (-radius) ang
    v3      = orthoVec radius (-radius) ang

    v4      = orthoVec (radius + rl) (-radius) ang
    v5      = orthoVec circum (-rl) ang
    v6      = orthoVec circum 0 ang




-- | 'minorCircleSweepCW' : @ angle * radius * inclination -> CatTrail @
--
-- > ang should be in the range 0 < ang <= 90deg.
--
minorCircleSweepCW :: (Real u, Floating u)
                   => Radian -> u -> Radian -> CatTrail u
minorCircleSweepCW ang radius theta = 
    catcurve (pvec p0 p1) (pvec p1 p2) (pvec p2 p3)
  where
    kfactor = fromRadian $ ang / (0.5*pi)
    rl      = kfactor * radius * kappa
    totang  = circularModulo $ theta + (half_pi - ang)

    p0      = displace (theta_up    radius theta) zeroPt
    p1      = displace (theta_right rl     theta) p0
    p2      = displace (theta_up    rl     totang) p3
    p3      = displace (avec totang radius) zeroPt


-- | 'minorCircleSweepCCW' : @ angle * radius * inclination -> CatTrail @
--
-- > ang should be in the range 0 < ang <= 90deg.
--
minorCircleSweepCCW :: (Real u, Floating u)
                    => Radian -> u -> Radian -> CatTrail u
minorCircleSweepCCW ang radius theta = 
    catcurve (pvec p0 p1) (pvec p1 p2) (pvec p2 p3)
  where
    kfactor = fromRadian $ ang / (0.5*pi)
    rl      = kfactor * radius * kappa
    totang  = circularModulo $ theta - half_pi + ang

    p0      = displace (theta_down  radius theta) zeroPt
    p1      = displace (theta_right rl     theta) p0
    p2      = displace (theta_down  rl     totang) p3
    p3      = displace (avec totang radius) zeroPt



-- | 'circleSweepCW' : @ apex_angle * radius * inclination -> CatTrail @
--
-- > ang should be in the range 0 < ang < 360deg.
--
-- > if   0 < ang <=  90 returns 1 segment
-- > if  90 < ang <= 180 returns 2 segments
-- > if 180 < ang <= 270 returns 3 segments
-- > if 270 < ang <  360 returns 4 segmenets
--
circleSweepCW :: (Real u, Floating u)
              => Radian -> u -> Radian -> CatTrail u
circleSweepCW ang radius theta = go (circularModulo ang)
  where
    go a | a <= half_pi = wedge1 a
         | a <= pi      = wedge2 (a/2)
         | a <= 1.5*pi  = wedge3 (a/3)
         | otherwise    = wedge4 (a/4)
    
    wedge1 a =           minorCircleSweepCW a radius theta

    wedge2 a =           minorCircleSweepCW a radius theta
               `mappend` minorCircleSweepCW a radius (theta-a)

    wedge3 a =           minorCircleSweepCW a radius theta
               `mappend` minorCircleSweepCW a radius (theta - a)
               `mappend` minorCircleSweepCW a radius (theta - 2*a)
  
    wedge4 a =           minorCircleSweepCW a radius theta
               `mappend` minorCircleSweepCW a radius (theta - a)
               `mappend` minorCircleSweepCW a radius (theta - 2*a)
               `mappend` minorCircleSweepCW a radius (theta - 3*a)




-- | 'circleSweepCCW' : @ apex_angle * radius * inclination -> CatTrail @
--
-- > ang should be in the range 0 < ang < 360deg.
--
-- > if   0 < ang <=  90 returns 1 segment
-- > if  90 < ang <= 180 returns 2 segments
-- > if 180 < ang <= 270 returns 3 segments
-- > if 270 < ang <  360 returns 4 segmenets
--
circleSweepCCW :: (Real u, Floating u)
               => Radian -> u -> Radian -> CatTrail u
circleSweepCCW ang radius theta = go (circularModulo ang)
  where
    go a | a <= half_pi = wedge1 a
         | a <= pi      = wedge2 (a/2)
         | a <= 1.5*pi  = wedge3 (a/3)
         | otherwise    = wedge4 (a/4)
    
    wedge1 a =           minorCircleSweepCCW a radius theta

    wedge2 a =           minorCircleSweepCCW a radius theta
               `mappend` minorCircleSweepCCW a radius (theta+a)

    wedge3 a =           minorCircleSweepCCW a radius theta
               `mappend` minorCircleSweepCCW a radius (theta+a)
               `mappend` minorCircleSweepCCW a radius (theta+a+a)
  
    wedge4 a =           minorCircleSweepCCW a radius theta
               `mappend` minorCircleSweepCCW a radius (theta+a)
               `mappend` minorCircleSweepCCW a radius (theta+a+a)
               `mappend` minorCircleSweepCCW a radius (theta+a+a+a)


-- | inclination is the inclination of the chord.
--
circularArcCW :: (Real u, Floating u) => Radian -> u -> Radian -> CatTrail u 
circularArcCW apex_ang radius inclin = 
    circleSweepCW apex_ang radius (inclin + 0.5 * apex_ang)


-- | inclination is the inclination of the chord.
--
circularArcCCW :: (Real u, Floating u) => Radian -> u -> Radian -> CatTrail u 
circularArcCCW apex_ang radius inclin = 
    circleSweepCCW apex_ang radius (inclin - 0.5 * apex_ang)


-- | Proper semicircles do not make a good squiggle (it needs a 
-- bit of pinch).
--
squiggleWave :: (Real u, Floating u) => Int -> u -> Radian -> CatTrail u
squiggleWave i unit ang = mconcat $ replicate i $ squiggle1 unit ang
    
squiggle1 :: (Real u, Floating u) => u -> Radian -> CatTrail u
squiggle1 unit ang = 
              catcurve  v1            (vdiff v1 v2)   (vdiff v2 v3)
    `mappend` catcurve (vdiff v3 v4)  (vdiff v4 v5)   (vdiff v5 v6)
    `mappend` catcurve (vdiff v6 v7)  (vdiff v7 v8)   (vdiff v8 v9)
    `mappend` catcurve (vdiff v9 v10) (vdiff v10 v11) (vdiff v11 v12)
  where
    four_radius   = unit
    radius        = 0.25 * four_radius
    two_radius    = 0.5  * four_radius
    three_radius  = 0.75 * four_radius
    rl            = radius * kappa
    micro         = 0.33 * rl           -- seems good
    
    v1            = orthoVec micro rl ang
    v2            = orthoVec (radius - rl) radius ang
    v3            = orthoVec radius radius ang

    v4            = orthoVec (radius + rl) radius ang
    v5            = orthoVec (two_radius - micro) rl ang
    v6            = orthoVec two_radius  0 ang

    v7            = orthoVec (two_radius + micro) (-rl) ang
    v8            = orthoVec (three_radius - rl) (-radius) ang
    v9            = orthoVec three_radius (-radius) ang

    v10           = orthoVec (three_radius + rl) (-radius) ang
    v11           = orthoVec (four_radius - micro) (-rl) ang
    v12           = orthoVec four_radius 0 ang

    

squareWave :: Floating u => Int -> u -> Radian -> CatTrail u 
squareWave n unit ang 
    | n >  0    = monPreRepeatPost up_half (n - 1,kont) fin
    | otherwise = mempty
  where
    up_half     = catline $ theta_up    (0.25 * unit) ang
    up_one      = catline $ theta_up    (0.5  * unit) ang
    down_one    = catline $ theta_down  (0.5  * unit) ang
    right_half  = catline $ theta_right (0.5  * unit) ang

    kont        = right_half `mappend` down_one `mappend` right_half
                             `mappend` up_one

    fin         = right_half `mappend` down_one `mappend` right_half
                             `mappend` up_half




-- |
--  
sawtoothWave :: (Real u, Floating u) => Int -> u -> Radian -> CatTrail u 
sawtoothWave n unit ang 
    | n >  0    = monPreRepeatPost up_half (n - 1,kont) fin
    | otherwise = mempty
  where
    up_half  = catline $ theta_up_right (0.25 * unit) ang
    up_one   = catline $ theta_up_right (0.5  * unit) ang
    down_one = catline $ theta_down_right (0.5 * unit) ang

    kont     = down_one `mappend` up_one
    fin      = down_one `mappend` up_half



semicircleWave :: (Real u, Floating u) 
               => ClockDirection -> Int -> u -> Radian -> CatTrail u
semicircleWave cdir i unit ang = 
    mconcat $ replicate i $ fn cdir (avec ang unit)
  where
    fn CCW = semicircleCCW
    fn _   = semicircleCW



--------------------------------------------------------------------------------

-- | 'tricurve' : @ clock_direction * base_width * height * 
--      base_inclination -> CatTrail @
-- 
-- Curve in a triangle - base_width and height are expected to 
-- be positive.
-- 
tricurve :: Floating u => ClockDirection -> u -> u -> Radian -> CatTrail u
tricurve CW  bw h ang = ctriCW bw h ang
tricurve CCW bw h ang = ctriCW bw (-h) ang


-- | Curve in a triangle.
-- 
ctriCW :: Floating u => u -> u -> Radian -> CatTrail u
ctriCW bw h ang = catcurve v1 zeroVec v2
  where
    v1 = orthoVec (0.5 * bw) h ang
    v2 = orthoVec (0.5 * bw) (-h) ang


-- | 'rectcurve' : @ clock_direction * base_width * height * 
--      base_inclination -> CatTrail @
-- 
-- Curve in a rectangle.
-- 
rectcurve :: Floating u => ClockDirection -> u -> u -> Radian -> CatTrail u
rectcurve CW  bw h ang = crectCW bw h ang
rectcurve CCW bw h ang = crectCW bw (-h) ang


-- | Curve in a rectangle.
-- 
crectCW :: Floating u => u -> u -> Radian -> CatTrail u
crectCW bw h ang = catcurve v1 v2 v3
  where
    v1 = orthoVec 0    h  ang
    v2 = orthoVec bw   0  ang
    v3 = orthoVec 0  (-h) ang



-- | Curve in a trapezium.
-- 
trapcurve :: Floating u 
          => ClockDirection -> u -> u -> Radian -> Radian -> CatTrail u
trapcurve CW  bw h interior_ang ang = ctrapCW  bw h interior_ang ang
trapcurve CCW bw h interior_ang ang = ctrapCCW bw h interior_ang ang

-- | Curve in a trapezium (CW).
-- 
-- h must be positive.
--
ctrapCW :: Floating u => u -> u -> Radian -> Radian -> CatTrail u
ctrapCW bw h interior_ang ang = catcurve v1 v2 v3
  where
    minor_bw = h / (fromRadian $ tan interior_ang)
    v1       = orthoVec minor_bw                h  ang
    v2       = orthoVec (bw - (2 * minor_bw))   0  ang
    v3       = orthoVec minor_bw              (-h) ang

-- | Curve in a trapezium (CCW).
-- 
-- h must be positive.
--
ctrapCCW :: Floating u => u -> u -> Radian -> Radian -> CatTrail u
ctrapCCW bw h interior_ang ang = catcurve v1 v2 v3
  where
    minor_bw = h / (fromRadian $ tan interior_ang)
    v1       = orthoVec minor_bw              (-h)  ang
    v2       = orthoVec (bw - (2 * minor_bw))   0  ang
    v3       = orthoVec minor_bw                h ang

-- | Curve in half a /bowtie/.
-- 
bowcurve :: Floating u 
         => ClockDirection -> u -> u -> Radian -> CatTrail u
bowcurve CW  bw h ang = cbowCW bw h ang
bowcurve CCW bw h ang = cbowCW bw (-h) ang

-- | Curve in half a /bowtie/.
-- 
cbowCW :: Floating u => u -> u -> Radian -> CatTrail u
cbowCW bw h ang = catcurve v1 v2 v3
  where
    v1 = orthoVec 0    h  ang
    v2 = orthoVec bw (-h)  ang
    v3 = orthoVec 0    h ang


-- | Wedge curve formed inside a bowtie rotated by 90deg.
-- 
wedgecurve :: Floating u 
           => ClockDirection -> u -> u -> Radian -> CatTrail u
wedgecurve CW  bw h ang = cwedgeCW bw h ang
wedgecurve CCW bw h ang = cwedgeCW bw (-h) ang

-- | Wedge curve clockwise.
-- 
cwedgeCW :: Floating u => u -> u -> Radian -> CatTrail u
cwedgeCW bw h ang = catcurve v1 v2 v3
  where
    v1 = orthoVec   bw    h  ang
    v2 = orthoVec (-bw)   0  ang
    v3 = orthoVec   bw  (-h) ang


-- | Variation of wedge curve that draws a loop.
-- 
loopcurve :: Floating u 
          => ClockDirection -> u -> u -> Radian -> CatTrail u
loopcurve CW  bw h ang = cloopCW bw h ang
loopcurve CCW bw h ang = cloopCW bw (-h) ang


-- | loop curve clockwise.
-- 
cloopCW :: Floating u => u -> u -> Radian -> CatTrail u
cloopCW bw h ang = catcurve v1 v2 v3
  where
    ww = 2.0 * bw 
    v1 = orthoVec  (1.5 * bw)    h  ang
    v2 = orthoVec  (-ww)         0  ang
    v3 = orthoVec  (1.5 * bw)  (-h) ang
