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

    ocircle
  , ochar
  , ocharUpright
  , ocharDescender
  , ocurrency
  , left_slice
  , right_slice

  , left_triangle
  , right_triangle

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
-- expected to be redefined elsewhere at fixed size and not used 
-- in /user code/. 
-- 
-- Using camelCase here, then under_score in the DocText versions
-- adds double the function signatures to the Haddock docs.
--



ocircle :: InterpretUnit u => u -> LocGraphic u
ocircle radius = dcDisk DRAW_STROKE radius

-- | Note this looks horrible for chars with descenders.
--
ochar :: (Fractional u, InterpretUnit u) 
             => EscapedChar -> LocGraphic u
ochar esc = char1 <> circ1
  where
    char1 = runPosObject CENTER $ posEscChar esc
    circ1 = localize (set_line_width 0.75) $ capHeight >>= \h -> ocircle (0.85 * h)

ocharUpright :: (Fractional u, InterpretUnit u) 
             => EscapedChar -> LocGraphic u
ocharUpright esc = char1 <> circ1
  where
    char1 = runPosObject CENTER $ posEscCharUpright esc
    circ1 = localize (set_line_width 0.75) $ capHeight >>= \h -> ocircle (0.85 * h)

ocharDescender :: (Fractional u, InterpretUnit u) 
             => EscapedChar -> LocGraphic u
ocharDescender esc = char1 <> circ1
  where
    char1 = fmap abs descender >>= \dy -> 
            moveStart (go_up dy) $ runPosObject CENTER $ posEscCharUpright esc
    circ1 = localize (set_line_width 0.75) $ capHeight >>= \h -> ocircle (0.85 * h)


ocurrency :: (Floating u, InterpretUnit u) 
          => u -> LocGraphic u 
ocurrency ra = ocircle ra <> lne <> lnw <> lsw <> lse
  where
    ra3 = 0.33 * ra
    lne = moveStart (go_north_east ra) $ locStraightLine $ go_north_east ra3
    lnw = moveStart (go_north_west ra) $ locStraightLine $ go_north_west ra3
    lsw = moveStart (go_south_west ra) $ locStraightLine $ go_south_west ra3
    lse = moveStart (go_south_east ra) $ locStraightLine $ go_south_east ra3

left_slice :: (Real u, Floating u, InterpretUnit u) 
          => u -> LocGraphic u
left_slice radius = moveStart (go_left $ 0.5 * radius) lwedge
  where
    lwedge = supplyIncline 0 $ wedge DRAW_STROKE radius quarter_pi


right_slice :: (Real u, Floating u, InterpretUnit u) 
          => u -> LocGraphic u
right_slice radius = moveStart (go_right $ 0.5 * radius) rwedge
  where
    rwedge = supplyIncline pi $ wedge DRAW_STROKE radius quarter_pi

left_triangle :: (Fractional u, InterpretUnit u) 
              => u -> LocGraphic u
left_triangle w = 
    drawPlacedTrail CSTROKE $ placeCatTrail (go_left $ 0.5 * w)
                            $ line_r <> vbase <> line_l
  where
    hh     = 0.40 * w
    line_r = catline $ vec w hh
    vbase  = catline $ go_down $ 2*hh
    line_l = catline $ vec (-w) hh

right_triangle :: (Fractional u, InterpretUnit u) 
               => u -> LocGraphic u
right_triangle w = 
    drawPlacedTrail CSTROKE $ placeCatTrail (go_right $ 0.5 * w)
                            $ line_l <> vbase <> line_r
  where
    hh     = 0.40 * w
    line_l = catline $ vec (-w) hh
    vbase  = catline $ go_down $ 2*hh
    line_r = catline $ vec w hh


empty_box :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
empty_box w = drawPlacedTrail CSTROKE $ rectangleTrail w w

hbar :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
hbar u = 
    drawPlacedTrail OSTROKE $ placeCatTrail (go_left $ 0.5 * u) $ trail_right u

vbar :: (Fractional u, InterpretUnit u) => u -> LocGraphic u
vbar u = 
    drawPlacedTrail OSTROKE $ placeCatTrail (go_down $ 0.5 * u) $ trail_up u

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
