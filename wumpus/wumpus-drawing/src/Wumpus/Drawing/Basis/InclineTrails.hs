{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.InclineTrails
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.InclineTrails
  (

    incline_circle
  , incline_square
  , incline_rect
  , incline_diamond
  , incline_triangle
  , incline_barb
  , incline_tube
  , incline_chamf_rect

  , trail_diagh
  , trail_diagv
  , trail_hdiag
  , trail_vdiag

  , trail_hdiagh
  , trail_vdiagv

  , trail_perp_bar
  , trail_perp_bar2

  , trail_vflam
  , trail_ortho_hbar
  , trail_ortho_vbar

  , trail_hright
  , trail_vright

  , trail_hrr
  , trail_vrr
  , trail_rrh
  , trail_rrv

  , trail_rect_loop

  , vtriCurve
  , vrectCurve
  , vtrapCurve
  , vbowCurve
  , vwedgeCurve

  )
  where

import Wumpus.Drawing.Basis.DrawingPrimitives

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Data.Monoid


-- Shapes...

incline_circle :: (Real u, Floating u) => Vec2 u -> AnaTrail u
incline_circle v1 = 
    anaCatTrail zeroVec (semicircleTrail CW v1 <> semicircleTrail CW rv1)
  where
    rv1 = vreverse v1

incline_square :: (Real u, Floating u) => Vec2 u -> AnaTrail u
incline_square v1 = incline_rect (vlength v1) v1


incline_rect :: (Real u, Floating u) => u -> Vec2 u -> AnaTrail u
incline_rect h v1 = 
    anaCatTrail (theta_down (0.5 * h) ang) catt
  where
    len  = vlength v1
    ang  = vdirection v1
    catt = mconcat [ trail_theta_right len ang
                   , trail_theta_up h ang 
                   , trail_theta_left len ang 
                   , trail_theta_down h ang 
                   ]
           
incline_diamond :: (Real u, Floating u) => u -> Vec2 u -> AnaTrail u
incline_diamond h v1 = anaCatTrail zeroVec catt
  where
    hw   = 0.5 * vlength v1
    hh   = 0.5 * h
    ang  = vdirection v1
    catt = mconcat [ orthoCatTrail   hw  (-hh) ang
                   , orthoCatTrail   hw    hh  ang 
                   , orthoCatTrail (-hw)   hh  ang 
                   , orthoCatTrail (-hw) (-hh) ang 
                   ]

-- | Note - vector represents midpoint of the baseline to the 
-- tip. Angle is the ang of the tip.
--
-- This trail is primarily for drawing arrowheads.
-- 
incline_triangle :: (Real u, Floating u) => Radian -> Vec2 u -> AnaTrail u
incline_triangle tip_ang v1 = 
    anaCatTrail (theta_up opposite theta) catt
  where
    half_ang = 0.5 * tip_ang
    theta    = vdirection v1
    h        = vlength v1
    opposite = h * (fromRadian $ tan half_ang)

    catt     = mconcat [ trail_theta_adj_grazing h half_ang theta
                       , trail_theta_bkwd_adj_grazing h half_ang theta
                       , trail_theta_up (2 * opposite) theta
                       ]


-- | Note - vector represents midpoint of the baseline to the 
-- tip. Angle is the ang of the tip.
--
-- This trail is primarily for drawing arrowheads. The resulting 
-- path is /open/. 
-- 
incline_barb :: (Real u, Floating u) => Radian -> Vec2 u -> AnaTrail u
incline_barb tip_ang v1 = 
    anaCatTrail (theta_up opposite theta) catt
  where
    half_ang = 0.5 * tip_ang
    theta    = vdirection v1
    h        = vlength v1
    opposite = h * (fromRadian $ tan half_ang)

    catt     = mconcat [ trail_theta_adj_grazing h half_ang theta
                       , trail_theta_bkwd_adj_grazing h half_ang theta
                       ]


-- | @v1@ is the /interior/ vector.
--
incline_tube :: (Real u, Floating u) => u -> Vec2 u -> AnaTrail u
incline_tube h v1 = 
    anaCatTrail (theta_down_right hh ang) $ mconcat $
      [ trail_theta_right base_len ang
      , semicircleTrail CCW vup
      , trail_theta_left base_len ang
      , semicircleTrail CCW vdown
      ]
  where
    hh        = 0.5 * h
    ang       = vdirection v1 
    base_len  = vlength v1 - h
    vup       = avec (ang + half_pi) h
    vdown     = avec (ang - half_pi) h


incline_chamf_rect :: (Real u, Floating u) => u -> Vec2 u -> AnaTrail u
incline_chamf_rect h v1 = 
    anaCatTrail zeroVec $ mconcat $
      [ trail_theta_down_right hh ang
      , trail_theta_right base_len ang
      , trail_theta_up_right hh ang
      , trail_theta_up_left hh ang
      , trail_theta_left base_len ang
      , trail_theta_down_left hh ang
      ]
  where
    hh        = 0.5 * h
    ang       = vdirection v1 
    base_len  = vlength v1 - h

-- | Diagonal-horizontal trail.
--
-- >    --@
-- >   /
-- >  o
-- 
trail_diagh :: (Real u, Floating u) => u -> Vec2 u -> CatTrail u
trail_diagh leg v1 = 
    let h   = vector_y v1
        mid = (abs $ vector_x v1) - leg
    in case horizontalDirection $ vdirection v1 of
         RIGHTWARDS -> orthoCatTrail mid h 0    <> trail_right leg
         _          -> orthoCatTrail (-mid) h 0 <> trail_left leg

trail_diagv :: (Real u, Floating u) => u -> Vec2 u -> CatTrail u
trail_diagv leg v1 = 
    let w   = vector_x v1
        mid = (abs $ vector_y v1) - leg
    in case verticalDirection $ vdirection v1 of
         UPWARDS -> orthoCatTrail w mid 0 <> trail_up leg
         _       -> orthoCatTrail w (-mid) 0 <> trail_down leg


trail_hdiag :: (Real u, Floating u) => u -> Vec2 u -> CatTrail u
trail_hdiag leg v1 = 
    let h   = vector_y v1
        mid = (abs $ vector_x v1) - leg
    in case horizontalDirection $ vdirection v1 of
         RIGHTWARDS -> trail_right leg <> orthoCatTrail mid h 0
         _          -> trail_left  leg <> orthoCatTrail (-mid) h 0


trail_vdiag :: (Real u, Floating u) => u -> Vec2 u -> CatTrail u
trail_vdiag leg v1 = 
    let w   = vector_x v1
        mid = (abs $ vector_y v1) - leg
    in case verticalDirection $ vdirection v1 of
         UPWARDS -> trail_up   leg <> orthoCatTrail w mid 0
         _       -> trail_down leg <> orthoCatTrail w (-mid) 0



-- | Horizontal-diagonal-horizontal trail.
--
-- >      --@
-- >     /
-- >  o--
-- 
--
trail_hdiagh :: (Real u, Floating u) => u -> u -> Vec2 u -> CatTrail u
trail_hdiagh legl legr v1 = 
    let h   = vector_y v1
        mid = (abs $ vector_x v1) - (legl + legr)
    in case horizontalDirection $ vdirection v1 of
         RIGHTWARDS -> mconcat [ trail_right legl
                               , orthoCatTrail mid h 0
                               , trail_right legr
                               ] 
         _          -> mconcat [ trail_left  legl
                               , orthoCatTrail (-mid) h 0
                               , trail_left legr
                               ] 



trail_vdiagv :: (Real u, Floating u) => u -> u -> Vec2 u -> CatTrail u
trail_vdiagv legl legr v1 = 
    let w   = vector_x v1
        mid = (abs $ vector_y v1) - (legl + legr)
    in case verticalDirection $ vdirection v1 of
         UPWARDS -> mconcat [ trail_up legl
                            , orthoCatTrail w mid 0
                            , trail_up legr
                            ] 
         _       -> mconcat [ trail_down  legl
                            , orthoCatTrail w (-mid) 0
                            , trail_down legr
                            ] 

-- | Uniform leg size.
--
trail_perp_bar :: (Real u, Floating u) 
               => ClockDirection -> u -> Vec2 u -> CatTrail u
trail_perp_bar CW h v1 = trail_perp_barCW h v1
trail_perp_bar _  h v1 = trail_perp_barCW (-h) v1


trail_perp_barCW :: (Real u, Floating u) => u -> Vec2 u -> CatTrail u
trail_perp_barCW h v1 = 
    trail_theta_up h ang <> catline v1 <> trail_theta_down h ang
  where
    ang = vdirection v1


-- | Bar connector - independent leg size, legs perpendicular.
--
-- 
-- >  o    @ 
-- >  |    |
-- >  '----'  
--
-- The bar is drawn /below/ the points.
--
trail_perp_bar2 :: (Real u, Floating u) 
                => ClockDirection -> u -> u -> Vec2 u -> CatTrail u
trail_perp_bar2 CW h1 h2 v1 = trail_perp_barCW2 h1 h2 v1
trail_perp_bar2 _  h1 h2 v1 = trail_perp_barCW2 (-h1) (-h2) v1


trail_perp_barCW2 :: (Real u, Floating u) => u -> u -> Vec2 u -> CatTrail u
trail_perp_barCW2 h1 h2 v1 = 
       trail_theta_up h1 ang 
    <> orthoCatTrail (vlength v1) (negate $ h1 - h2) ang
    <> trail_theta_down h2 ang
  where
    ang = vdirection v1


-- | Independent leg size.
--
trail_vflam :: (Real u, Floating u) 
            => ClockDirection -> u -> u -> Vec2 u -> CatTrail u
trail_vflam CW h1 h2 v1 = trail_vflamCW h1 h2 v1
trail_vflam _  h1 h2 v1 = trail_vflamCW (-h1) (-h2) v1


trail_vflamCW :: (Real u, Floating u) => u -> u -> Vec2 u -> CatTrail u
trail_vflamCW h1 h2 v1 = 
    diffLines [ p0, p0 .+^ vvec h1, p1 .+^ vvec h2, p1 ]
  where
    p0 = zeroPt
    p1 = p0 .+^ v1


-- | Height is minimum leg height. Ortho bar is horizontal.
--
trail_ortho_hbar :: (Real u, Floating u) 
                 => ClockDirection -> u -> Vec2 u -> CatTrail u
trail_ortho_hbar CW h v1 = trail_ortho_hbarCW  h v1
trail_ortho_hbar _  h v1 = trail_ortho_hbarCCW h v1


trail_ortho_hbarCW :: (Real u, Floating u) 
                   => u -> Vec2 u -> CatTrail u
trail_ortho_hbarCW ymin v1@(V2 x y) = case quadrant $ vdirection v1 of
    QUAD_NE -> trail_up ymaj <> trail_right x <> trail_down ymin
    QUAD_SE -> trail_up ymin <> trail_right x <> trail_down ymaj
    QUAD_NW -> trail_down ymin <> trail_left (abs x) <> trail_up ymaj
    QUAD_SW -> trail_down ymaj <> trail_left (abs x) <> trail_up ymin
  where
    ymaj = ymin + abs y




trail_ortho_hbarCCW :: (Real u, Floating u) 
                   => u -> Vec2 u -> CatTrail u
trail_ortho_hbarCCW ymin v1@(V2 x y) = case quadrant $ vdirection v1 of
    QUAD_NE -> trail_down ymin <> trail_right x <> trail_up ymaj
    QUAD_SE -> trail_down ymaj <> trail_right x <> trail_up ymin
    QUAD_NW -> trail_up ymaj <> trail_left (abs x) <> trail_down ymin
    QUAD_SW -> trail_up ymin <> trail_left (abs x) <> trail_down ymaj
  where
    ymaj = ymin + abs y




-- | Width is minimum leg width. Ortho bar is vertical.
--
trail_ortho_vbar :: (Real u, Floating u) 
                 => ClockDirection -> u -> Vec2 u -> CatTrail u
trail_ortho_vbar CW w v1 = trail_ortho_vbarCW  w v1
trail_ortho_vbar _  w v1 = trail_ortho_vbarCCW w v1


trail_ortho_vbarCW :: (Real u, Floating u) 
                   => u -> Vec2 u -> CatTrail u
trail_ortho_vbarCW xmin v1@(V2 x y) = case quadrant $ vdirection v1 of
    QUAD_NE -> trail_left xmin <> trail_up y <> trail_right xmaj
    QUAD_NW -> trail_left xmaj <> trail_up y <> trail_right xmin
    QUAD_SE -> trail_right xmaj <> trail_down (abs y) <> trail_left xmin
    QUAD_SW -> trail_right xmin <> trail_down (abs y) <> trail_left xmaj
  where
    xmaj = xmin + abs x



trail_ortho_vbarCCW :: (Real u, Floating u) 
                   => u -> Vec2 u -> CatTrail u
trail_ortho_vbarCCW xmin v1@(V2 x y) = case quadrant $ vdirection v1 of
    QUAD_NE -> trail_right xmaj <> trail_up y <> trail_left xmin
    QUAD_NW -> trail_right xmin <> trail_up y <> trail_left xmaj
    QUAD_SE -> trail_left  xmin <> trail_down (abs y) <> trail_right xmaj
    QUAD_SW -> trail_left  xmaj <> trail_down (abs y) <> trail_right xmin
  where
    xmaj = xmin + abs x


trail_hright :: Num u => Vec2 u -> CatTrail u
trail_hright (V2 x y) = trail_right x <> trail_up y


trail_vright :: Num u => Vec2 u -> CatTrail u
trail_vright (V2 x y) = trail_up y <> trail_right x

trail_hrr :: (Floating u, Ord u) => u -> Vec2 u -> CatTrail u
trail_hrr x1 (V2 x y) = 
       trail_theta_right x1 ang 
    <> trail_theta_up y ang 
    <> trail_theta_right (abs x  - x1) ang
  where
    ang = if x < 0 then pi else 0

trail_vrr :: (Floating u, Ord u) => u -> Vec2 u -> CatTrail u
trail_vrr y1 (V2 x y) = 
       trail_theta_up y1 ang 
    <> trail_theta_right x ang 
    <> trail_theta_up (abs y  - y1) ang
  where
    ang = if y < 0 then pi else 0


trail_rrh :: (Floating u, Ord u) => u -> Vec2 u -> CatTrail u
trail_rrh x1 (V2 x y) = 
       trail_theta_right (abs x  - x1) ang 
    <> trail_theta_up y ang 
    <> trail_theta_right x1 ang
  where
    ang = if x < 0 then pi else 0

trail_rrv :: (Floating u, Ord u) => u -> Vec2 u -> CatTrail u
trail_rrv y1 (V2 x y) = 
       trail_theta_up (abs y - y1) ang 
    <> trail_theta_right x ang 
    <> trail_theta_up y1 ang
  where
    ang = if y < 0 then pi else 0


trail_rect_loop :: (Real u, Floating u) 
                => ClockDirection -> u -> u -> u -> Vec2 u -> CatTrail u
trail_rect_loop CW = trail_rect_loopCW
trail_rect_loop _  = trail_rect_loopCCW

trail_rect_loopCW :: (Real u, Floating u) 
                  => u -> u -> u -> Vec2 u -> CatTrail u
trail_rect_loopCW extl extr h v1 = 
       trail_theta_left  extl ang
    <> trail_theta_up    h    ang
    <> trail_theta_right (len + extl + extr) ang
    <> trail_theta_down  h    ang
    <> trail_theta_left  extr ang
  where
    ang = vdirection v1
    len = vlength v1

trail_rect_loopCCW :: (Real u, Floating u)
                   => u -> u -> u -> Vec2 u -> CatTrail u
trail_rect_loopCCW extl extr h v1 = 
       trail_theta_left  extl ang
    <> trail_theta_down  h    ang
    <> trail_theta_right (len + extl + extr) ang
    <> trail_theta_up    h    ang
    <> trail_theta_left  extr ang
  where
    ang = vdirection v1
    len = vlength v1


-- | 'triCurve' formulated with a /base vector/ rather than 
-- base-width and angle of inclination.
--
vtriCurve :: (Real u, Floating u) 
          => ClockDirection -> u -> Vec2 u -> CatTrail u 
vtriCurve clk h v1 = triCurve clk (vlength v1) h (vdirection v1)
  

-- | 'rectCurve' formulated with a /base vector/ rather than 
-- base-width and angle of inclination.
--
vrectCurve :: (Real u, Floating u) 
           => ClockDirection -> u -> Vec2 u -> CatTrail u
vrectCurve clk h v1 = rectCurve clk (vlength v1) h (vdirection v1)


-- | 'trapCurve' formulated with a /base vector/ rather than 
-- base-width and angle of inclination.
--
vtrapCurve :: (Real u, Floating u)
           => ClockDirection -> u -> Radian -> Vec2 u -> CatTrail u
vtrapCurve clk h interior_ang v1 = 
    trapCurve clk (vlength v1) h interior_ang (vdirection v1)


-- | 'bowCurve' formulated with a /base vector/ rather than 
-- base-width and angle of inclination.
--
vbowCurve :: (Real u, Floating u)
          => ClockDirection -> u -> Vec2 u -> CatTrail u
vbowCurve clk h v1 = bowCurve clk (vlength v1) h (vdirection v1) 

-- | 'wedgeCurve' formulated with a /base vector/ rather than 
-- base-width and angle of inclination.
--
vwedgeCurve :: (Real u, Floating u)
            => ClockDirection -> u -> Vec2 u -> CatTrail u
vwedgeCurve clk h v1 = wedgeCurve clk (vlength v1) h (vdirection v1) 

