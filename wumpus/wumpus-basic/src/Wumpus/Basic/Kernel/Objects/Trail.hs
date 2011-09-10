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

  , ortholine
 
  , sineWaveTrail
  , semicircleAboveTrail
  , semicircleBelowTrail
  , squiggleTrail

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


-- | Alternative to @line@, specifying the vector components 
-- rather the vector itself.
--
ortholine :: Floating u => u -> u -> Radian -> CatTrail u 
ortholine x y ang = catline (orthoVec x y ang)


--------------------------------------------------------------------------------


-- | Helper
--
vdiff :: Num u => Vec2 u -> Vec2 u -> Vec2 u
vdiff  = flip (^-^)


-- | One-phase sine wave. Height is parametric.
--
sineWaveTrail :: (Real u, Floating u)
              => u -> Vec2 u -> CatTrail u
sineWaveTrail h base_vec = 
              catcurve  v1           (vdiff v1 v2)   (vdiff v2 v3)
    `mappend` catcurve (vdiff v3 v4)  (vdiff v4 v5)   (vdiff v5 v6)
    `mappend` catcurve (vdiff v6 v7)  (vdiff v7 v8)   (vdiff v8 v9)
    `mappend` catcurve (vdiff v9 v10) (vdiff v10 v11) (vdiff v11 v12)
  where
    base1 = vlength base_vec / 12
    h2    = h * (pi / 6)
    ang   = vdirection base_vec
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



-- | 'halfCircleA' : @ radius * center -> [Point] @ 
-- 
-- Make a circle from four Bezier curves. Although this function 
-- produces an approximation of a circle, the approximation seems
-- fine in practice.
--
semicircleAboveTrail :: (Real u, Floating u) => Vec2 u -> CatTrail u
semicircleAboveTrail base_vec =
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


-- | 'halfCircleB' : @ radius * center -> [Point] @ 
-- 
-- Make a circle from four Bezier curves. Although this function 
-- produces an approximation of a circle, the approximation seems
-- fine in practice.
--
semicircleBelowTrail :: (Real u, Floating u) => Vec2 u -> CatTrail u
semicircleBelowTrail base_vec =
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

-- | A good squiggle is not made from semicircles 
-- (it needs a bit of pinch).
--
squiggleTrail :: (Real u, Floating u) => Vec2 u -> CatTrail u
squiggleTrail base_vec = 
              catcurve  v1           (vdiff v1 v2) (vdiff v2 v3)
    `mappend` catcurve (vdiff v3 v4) (vdiff v4 v5) (vdiff v5 v6)
    `mappend` catcurve (vdiff v6 v7)  (vdiff v7 v8)   (vdiff v8 v9)
    `mappend` catcurve (vdiff v9 v10) (vdiff v10 v11) (vdiff v11 v12)
  where
    four_radius   = vlength base_vec
    radius        = 0.25 * four_radius
    two_radius    = 0.5  * four_radius
    three_radius  = 0.75 * four_radius
    ang           = vdirection base_vec
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
