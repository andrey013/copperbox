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
  , CatTrail(..)        -- temporarily exposed
  , PlacedTrail(..)     -- temporarily exposed

  , drawPlacedTrail
  , drawCatTrail

  , placeCatTrail
  
  , trailIterateLocus

  , placedTrailPoints

  , rectangleTrail
  , diamondTrail
  , polygonTrail

  
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

  , minorArcCW
  , minorArcCCW
  , arcTrailCW
  , arcTrailCCW

  , sineWave
  , sineWave1
  , squareWave
  , sawtoothWave
  , squiggleWave
  , semicircleWave

  , tricurve
  , rectcurve
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

data PlacedTrail u = PlacedTrail
      { pt_init_vec :: Vec2 u
      , pt_segments :: [TrailSegment u]
      }
  deriving (Eq,Ord,Show)

type instance DUnit (PlacedTrail u) = u

newtype CatTrail u = CatTrail { getCatTrail :: H (TrailSegment u) }

type instance DUnit (CatTrail u) = u


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



drawCatTrail :: InterpretUnit u => PathMode -> CatTrail u -> LocGraphic u
drawCatTrail mode (CatTrail ct) = promoteLoc $ \pt ->  
    normalizeCtxF pt >>= \dpt -> 
    liftQuery (mapM (fmap fn . normalizeCtxF) $ toListH ct) >>= \dxs -> 
    let pp = relPrimPath dpt dxs in dcPath mode pp
  where
    fn (TLine v1)        = relLineTo v1
    fn (TCurve v1 v2 v3) = relCurveTo v1 v2 v3


drawPlacedTrail :: InterpretUnit u => PathMode -> PlacedTrail u -> LocGraphic u
drawPlacedTrail mode (PlacedTrail v0 xs) = promoteLoc $ \pt -> 
    normalizeCtxF v0 >>= \dv0 -> 
    normalizeCtxF pt >>= \dpt -> 
    liftQuery (mapM (fmap fn . normalizeCtxF) xs) >>= \dxs -> 
    let pp = relPrimPath (dpt .+^ dv0) dxs in dcPath mode pp
  where
    fn (TLine v1)        = relLineTo v1
    fn (TCurve v1 v2 v3) = relCurveTo v1 v2 v3



-- | Turn a 'CatRail' into a 'PlacedTrail'.
--
placeCatTrail :: Vec2 u -> CatTrail u -> PlacedTrail u
placeCatTrail vinit cat = PlacedTrail { pt_init_vec = vinit
                                      , pt_segments = getCatTrail cat []
                                      }



-- | Create a PlacedTrail from the vector list - each vector in the 
-- input list iterates to the start point rather then the 
-- cumulative tip.
--
-- When the PlacedTrail is run, the supplied point is the /locus/ of 
-- the path and it does not form part of the path proper.
-- 
-- Like 'trailStartIsLocus', this constructor is typically used to 
-- make /shape paths/. Some shapes are easier to express as 
-- iterated displacements of the center rather than 
-- /turtle drawing/. 
-- 
trailIterateLocus :: Num u => [Vec2 u] -> PlacedTrail u
trailIterateLocus []      = PlacedTrail zeroVec []
trailIterateLocus (v0:xs) = PlacedTrail v0 (step v0 xs)
  where
    step v1 []      = [ TLine (v0 ^-^ v1) ]
    step v1 (v2:vs) = TLine (v2 ^-^ v1) : step v2 vs


placedTrailPoints :: InterpretUnit u => PlacedTrail u -> LocQuery u [Point2 u]
placedTrailPoints (PlacedTrail v0 ts) = qpromoteLoc $ \pt -> 
    return $ step (pt .+^ v0) ts
  where
    step p1 []                    = [p1]
    step p1 (TLine v1:xs)         = p1 : step (p1 .+^ v1) xs
    step p1 (TCurve v1 v2 v3 :xs) = let p2 = p1 .+^ v1
                                        p3 = p2 .+^ v2
                                        p4 = p3 .+^ v3 
                                    in p1 : p2 : p3 : step p4 xs

-- | 'rectangleTrail' : @ width * height -> PlacedTrail @
--
rectangleTrail :: Fractional u => u -> u -> PlacedTrail u
rectangleTrail w h = 
    PlacedTrail { pt_init_vec = ctr_to_bl 
                , pt_segments = map TLine spec
                }
  where
    ctr_to_bl = vec (negate $ 0.5*w) (negate $ 0.5*h)
    spec      = [ go_right w, go_up h, go_left w, go_down h ]




-- | 'diamondTrail' : @ half_width * half_height -> PlacedTrail @
--
diamondTrail :: Num u => u -> u -> PlacedTrail u
diamondTrail hw hh = trailIterateLocus [ vs,ve,vn,vw ]
  where
    vs = vvec (-hh)
    ve = hvec hw
    vn = vvec hh
    vw = hvec (-hw)


-- | 'polygonTrail' : @ num_points * radius -> PlacedTrail @ 
--
polygonTrail :: Floating u => Int -> u -> PlacedTrail u
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


-- | Helper
--
vdiff :: Num u => Vec2 u -> Vec2 u -> Vec2 u
vdiff  = flip (^-^)


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


-- DESIGN NOTE - maybe accounting for CW and CCW would make a 
-- better API than /above/ and /below/.


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




-- | 'minorArcCW' : @ angle * radius * inclination -> CatTrail @
--
-- > ang should be in the range 0 < ang <= 90deg.
--
minorArcCW :: (Real u, Floating u)
              => Radian -> u -> Radian -> CatTrail u
minorArcCW ang radius theta = 
    catcurve (pvec p0 p1) (pvec p1 p2) (pvec p2 p3)
  where
    kfactor = fromRadian $ ang / (0.5*pi)
    rl      = kfactor * radius * kappa
    totang  = circularModulo $ theta + (half_pi - ang)

    p0      = displace (theta_up    radius theta) zeroPt
    p1      = displace (theta_right rl     theta) p0
    p2      = displace (theta_up    rl     totang) p3
    p3      = displace (avec totang radius) zeroPt


-- | 'minorArcCCW' : @ angle * radius * inclination -> CatTrail @
--
-- > ang should be in the range 0 < ang <= 90deg.
--
minorArcCCW :: (Real u, Floating u)
              => Radian -> u -> Radian -> CatTrail u
minorArcCCW ang radius theta = 
    catcurve (pvec p0 p1) (pvec p1 p2) (pvec p2 p3)
  where
    kfactor = fromRadian $ ang / (0.5*pi)
    rl      = kfactor * radius * kappa
    totang  = circularModulo $ theta - half_pi + ang

    p0      = displace (theta_down  radius theta) zeroPt
    p1      = displace (theta_right rl     theta) p0
    p2      = displace (theta_down  rl     totang) p3
    p3      = displace (avec totang radius) zeroPt



-- | 'arcTrailCW' : @ apex_angle * radius * inclination -> CatTrail @
--
-- > ang should be in the range 0 < ang < 360deg.
--
-- > if   0 < ang <=  90 returns 1 segment
-- > if  90 < ang <= 180 returns 2 segments
-- > if 180 < ang <= 270 returns 3 segments
-- > if 270 < ang <  360 returns 4 segmenets
--
arcTrailCW :: (Real u, Floating u)
            => Radian -> u -> Radian -> CatTrail u
arcTrailCW ang radius theta = go (circularModulo ang)
  where
    go a | a <= half_pi = wedge1 a
         | a <= pi      = wedge2 (a/2)
         | a <= 1.5*pi  = wedge3 (a/3)
         | otherwise    = wedge4 (a/4)
    
    wedge1 a =           minorArcCW a radius theta

    wedge2 a =           minorArcCW a radius theta
               `mappend` minorArcCW a radius (theta-a)

    wedge3 a =           minorArcCW a radius theta
               `mappend` minorArcCW a radius (theta - a)
               `mappend` minorArcCW a radius (theta - 2*a)
  
    wedge4 a =           minorArcCW a radius theta
               `mappend` minorArcCW a radius (theta - a)
               `mappend` minorArcCW a radius (theta - 2 *a)
               `mappend` minorArcCW a radius (theta - 3 *a)




-- | 'arcTrailCCW' : @ apex_angle * radius * inclination -> CatTrail @
--
-- > ang should be in the range 0 < ang < 360deg.
--
-- > if   0 < ang <=  90 returns 1 segment
-- > if  90 < ang <= 180 returns 2 segments
-- > if 180 < ang <= 270 returns 3 segments
-- > if 270 < ang <  360 returns 4 segmenets
--
arcTrailCCW :: (Real u, Floating u)
            => Radian -> u -> Radian -> CatTrail u
arcTrailCCW ang radius theta = go (circularModulo ang)
  where
    go a | a <= half_pi = wedge1 a
         | a <= pi      = wedge2 (a/2)
         | a <= 1.5*pi  = wedge3 (a/3)
         | otherwise    = wedge4 (a/4)
    
    wedge1 a =           minorArcCCW a radius theta

    wedge2 a =           minorArcCCW a radius theta
               `mappend` minorArcCCW a radius (theta+a)

    wedge3 a =           minorArcCCW a radius theta
               `mappend` minorArcCCW a radius (theta+a)
               `mappend` minorArcCCW a radius (theta+a+a)
  
    wedge4 a =           minorArcCCW a radius theta
               `mappend` minorArcCCW a radius (theta+a)
               `mappend` minorArcCCW a radius (theta+a+a)
               `mappend` minorArcCCW a radius (theta+a+a+a)



-- | Proper semicircles do not make a good squiggle (it needs a 
-- bit of pinch).
--
squiggleWave :: (Real u, Floating u) => Int -> u -> Radian -> CatTrail u
squiggleWave i unit ang = mconcat $ replicate i $ squiggle1 unit ang
    
squiggle1 :: (Real u, Floating u) => u -> Radian -> CatTrail u
squiggle1 unit ang = 
              catcurve  v1           (vdiff v1 v2) (vdiff v2 v3)
    `mappend` catcurve (vdiff v3 v4) (vdiff v4 v5) (vdiff v5 v6)
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
               => ClockDirection ->  Int -> u -> Radian -> CatTrail u
semicircleWave cdir i unit ang = 
    mconcat $ replicate i $ fn cdir (avec ang unit)
  where
    fn CCW = semicircleCCW
    fn _   = semicircleCW



--------------------------------------------------------------------------------



-- | Curve in a triangle.
-- 
tricurve :: Floating u => u -> u -> Radian -> CatTrail u
tricurve bw h ang = catcurve v1 zeroVec v2
  where
    v1 = orthoVec (0.5 * bw) h ang
    v2 = orthoVec (0.5 * bw) (-h) ang



-- | Curve in a rectangle.
-- 
rectcurve :: Floating u => u -> u -> Radian -> CatTrail u
rectcurve bw h ang = catcurve v1 v2 v3
  where
    v1 = orthoVec 0    h  ang
    v2 = orthoVec bw   0  ang
    v3 = orthoVec 0  (-h) ang



-- | Curve in half a /bowtie/.
-- 
bowcurve :: Floating u => u -> u -> Radian -> CatTrail u
bowcurve bw h ang = catcurve v1 v2 v3
  where
    v1 = orthoVec 0    h  ang
    v2 = orthoVec bw (-h)  ang
    v3 = orthoVec 0    h ang


-- | Wedge curve formed inside a bowtie rotated by 90deg.
-- 
wedgecurve :: Floating u => u -> u -> Radian -> CatTrail u
wedgecurve bw h ang = catcurve v1 v2 v3
  where
    v1 = orthoVec   bw    h  ang
    v2 = orthoVec (-bw)   0  ang
    v3 = orthoVec   bw  (-h) ang


-- | Variation of wedge curve that draws a loop.
-- 
loopcurve :: Floating u => u -> u -> Radian -> CatTrail u
loopcurve bw h ang = catcurve v1 v2 v3
  where
    ww = 2.0 * bw 
    v1 = orthoVec  (1.5 * bw)    h  ang
    v2 = orthoVec  (-ww)         0  ang
    v3 = orthoVec  (1.5 * bw)  (-h) ang
