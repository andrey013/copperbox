{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.Symbols
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Symbols - many symbols expected to be re-defined as Dots or
-- character size PosObjects for DocText.
--
-- \*\* WARNING \*\* - naming conventention is to be determined...
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.Symbols
  (

    scircle
  , fcircle
  , fscircle
    
  , ssquare
  , fsquare
  , fssquare

  , sleft_slice
  , fleft_slice
  , fsleft_slice

  , sright_slice
  , fright_slice
  , fsright_slice

  , sleft_triangle
  , fleft_triangle
  , fsleft_triangle

  , sright_triangle
  , fright_triangle
  , fsright_triangle

  , ochar
  , ocharUpright
  , ocharDescender
  , ocurrency

  , empty_box

  , hbar
  , vbar
  , dbl_hbar
  , dbl_vbar

  )
  where

import Wumpus.Drawing.Basis.DrawingPrimitives

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

-- import Data.AffineSpace                         -- package: vector-space

--
-- DESIGN NOTE 
--
-- Should names follow the /function scheme/ (camelCase), or the
-- /data scheme/ (underscore_separators).
--
-- Objects here are functions as they take size param, but they
-- expected to be redefined elsewhere (possibly at fixed size) 
-- with the re-definitions used in /user code/ rather than these 
-- primitives. 
-- 
-- Using camelCase here, then under_score in the DocText versions
-- adds double the function signatures to the Haddock docs.
--
-- Also - should the names encode start pos, or can all start 
-- positions be center?
--

--
-- TikZ uses o to indicated /circled/.
--
-- Using o as a prefix for /open/ i.e. stroked has the problem 
-- that there aren\'t any other characters ideographic for filled
-- or filled stroked.
--

-- scircle
-- fcircle
-- fscircle



-- | Stroked circle.
-- 
-- Start pos - center.
--
scircle :: InterpretUnit u => u -> LocGraphic u
scircle radius = dcDisk DRAW_STROKE radius

-- | Filled circle.
-- 
-- Start pos - center.
--
fcircle :: InterpretUnit u => u -> LocGraphic u
fcircle radius = dcDisk DRAW_FILL radius

-- | Filled-stroked circle.
-- 
-- Start pos - center.
--
fscircle :: InterpretUnit u => u -> LocGraphic u
fscircle radius = dcDisk DRAW_FILL_STROKE radius




-- | Stroked square.
-- 
-- Start pos - center.
--
ssquare :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
ssquare w = renderAnaTrail CSTROKE $ rectangleTrail w w 


-- | Filled square.
-- 
-- Start pos - center.
--
fsquare :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
fsquare w = renderAnaTrail CFILL $ rectangleTrail w w 


-- | Filled-stroked square.
-- 
-- Start pos - center.
--
fssquare :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
fssquare w = renderAnaTrail CFILL_STROKE $ rectangleTrail w w 



-- | Implementation.
--
lslice :: (Real u, Floating u, InterpretUnit u) 
       => DrawMode -> u -> LocGraphic u
lslice mode radius = moveStart (go_left $ 0.5 * radius) lwedge
  where
    lwedge = supplyIncline 0 $ wedge mode radius quarter_pi

-- | Stroked left slice (wedge).
-- 
-- Start pos - ....
--
sleft_slice :: (Real u, Floating u, InterpretUnit u) 
            => u -> LocGraphic u
sleft_slice = lslice DRAW_STROKE


-- | Filled left slice (wedge).
-- 
-- Start pos - ....
--
fleft_slice :: (Real u, Floating u, InterpretUnit u) 
            => u -> LocGraphic u
fleft_slice = lslice DRAW_FILL


-- | Filled-stroked left slice (wedge).
-- 
-- Start pos - ....
--
fsleft_slice :: (Real u, Floating u, InterpretUnit u) 
             => u -> LocGraphic u
fsleft_slice = lslice DRAW_FILL_STROKE



-- | Implementation.
--
rslice :: (Real u, Floating u, InterpretUnit u) 
       => DrawMode -> u -> LocGraphic u
rslice mode radius = moveStart (go_right $ 0.5 * radius) rwedge
  where
    rwedge = supplyIncline pi $ wedge mode radius quarter_pi


-- | Stroked right slice (wedge).
-- 
-- Start pos - ....
--
sright_slice :: (Real u, Floating u, InterpretUnit u) 
             => u -> LocGraphic u
sright_slice = rslice DRAW_STROKE


-- | Filled right slice (wedge).
-- 
-- Start pos - ....
--
fright_slice :: (Real u, Floating u, InterpretUnit u) 
             => u -> LocGraphic u
fright_slice = rslice DRAW_FILL


-- | Filled-stroked right slice (wedge).
-- 
-- Start pos - ....
--
fsright_slice :: (Real u, Floating u, InterpretUnit u) 
              => u -> LocGraphic u
fsright_slice = rslice DRAW_FILL_STROKE



-- | Implementation.
--
left_tri :: (Fractional u, InterpretUnit u) 
         => PathMode -> u -> LocGraphic u
left_tri mode w = 
    renderAnaTrail mode $ anaCatTrail (go_left $ 0.5 * w)
                            $ line_r <> vbase <> line_l
  where
    hh     = 0.40 * w
    line_r = catline $ vec w hh
    vbase  = catline $ go_down $ 2*hh
    line_l = catline $ vec (-w) hh


-- | Stroked left triangle.
-- 
-- Start pos - ....
--
sleft_triangle :: (Real u, Floating u, InterpretUnit u) 
                => u -> LocGraphic u
sleft_triangle = left_tri CSTROKE


-- | Filled left triangle.
-- 
-- Start pos - ....
--
fleft_triangle :: (Real u, Floating u, InterpretUnit u) 
                => u -> LocGraphic u
fleft_triangle = left_tri CFILL


-- | Filled-stroked left triangle.
-- 
-- Start pos - ....
--
fsleft_triangle :: (Real u, Floating u, InterpretUnit u) 
                 => u -> LocGraphic u
fsleft_triangle = left_tri CFILL_STROKE


-- | Implementation
--
right_tri :: (Fractional u, InterpretUnit u) 
          => PathMode -> u -> LocGraphic u
right_tri mode w = 
    renderAnaTrail mode $ anaCatTrail (go_right $ 0.5 * w)
                            $ line_l <> vbase <> line_r
  where
    hh     = 0.40 * w
    line_l = catline $ vec (-w) hh
    vbase  = catline $ go_down $ 2*hh
    line_r = catline $ vec w hh


-- | Stroked right triangle.
-- 
-- Start pos - ....
--
sright_triangle :: (Real u, Floating u, InterpretUnit u) 
                => u -> LocGraphic u
sright_triangle = right_tri CSTROKE


-- | Filled right triangle.
-- 
-- Start pos - ....
--
fright_triangle :: (Real u, Floating u, InterpretUnit u) 
                => u -> LocGraphic u
fright_triangle = right_tri CFILL


-- | Filled-stroked right triangle.
-- 
-- Start pos - ....
--
fsright_triangle :: (Real u, Floating u, InterpretUnit u) 
                 => u -> LocGraphic u
fsright_triangle = right_tri CFILL_STROKE



-- | Note this looks horrible for chars with descenders.
--
ochar :: (Fractional u, InterpretUnit u) 
      => EscapedChar -> LocGraphic u
ochar esc = char1 <> circ1
  where
    char1 = runPosObject CENTER $ posEscChar esc
    circ1 = localize (set_line_width 0.75) $ capHeight >>= \h -> scircle (0.85 * h)

ocharUpright :: (Fractional u, InterpretUnit u) 
             => EscapedChar -> LocGraphic u
ocharUpright esc = char1 <> circ1
  where
    char1 = runPosObject CENTER $ posEscCharUpright esc
    circ1 = localize (set_line_width 0.75) $ capHeight >>= \h -> scircle (0.85 * h)

ocharDescender :: (Fractional u, InterpretUnit u) 
             => EscapedChar -> LocGraphic u
ocharDescender esc = char1 <> circ1
  where
    char1 = fmap abs descender >>= \dy -> 
            moveStart (go_up dy) $ runPosObject CENTER $ posEscCharUpright esc
    circ1 = localize (set_line_width 0.75) $ capHeight >>= \h -> scircle (0.85 * h)


ocurrency :: (Floating u, InterpretUnit u) 
          => u -> LocGraphic u 
ocurrency ra = scircle ra <> lne <> lnw <> lsw <> lse
  where
    ra3 = 0.33 * ra
    lne = moveStart (go_north_east ra) $ locStraightLine $ go_north_east ra3
    lnw = moveStart (go_north_west ra) $ locStraightLine $ go_north_west ra3
    lsw = moveStart (go_south_west ra) $ locStraightLine $ go_south_west ra3
    lse = moveStart (go_south_east ra) $ locStraightLine $ go_south_east ra3



empty_box :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
empty_box w = renderAnaTrail CSTROKE $ rectangleTrail w w

hbar :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
hbar u = 
    renderAnaTrail OSTROKE $ anaCatTrail (go_left $ 0.5 * u) $ trail_right u

vbar :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
vbar u = 
    renderAnaTrail OSTROKE $ anaCatTrail (go_down $ 0.5 * u) $ trail_up u

dbl_hbar :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
dbl_hbar u = line1 <> line2
  where
    line1 = moveStart (go_up $ 0.1 * u) $ hbar u
    line2 = moveStart (go_down $ 0.1 * u) $ hbar u


dbl_vbar :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
dbl_vbar u = line1 <> line2
  where
    line1 = moveStart (go_left $ 0.1 * u) $ vbar u
    line2 = moveStart (go_right $ 0.1 * u) $ vbar u
