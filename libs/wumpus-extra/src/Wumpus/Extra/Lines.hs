{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Lines
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Line drawing from vertex lists as-per OpenGL...
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Lines
  ( 

  -- * Dots
    dots
  , linesUnconnected
  , lineStrip
  , lineLoop
  , triangles
  , triangleStrip
  , triangleFan
  , quads
  , quadStrip 

  -- 
  , bend
  , tighten
  , tildeCurve
  , strline
  , strlineMidpt
  ) where



import Wumpus.Core

import Wumpus.Extra.Base
import Wumpus.Extra.BasicObjects

import Data.AffineSpace
import Data.VectorSpace








dots :: (Fractional u, Ord u) 
     => (Point2 u -> Picture u) -> [Point2 u] -> Picture u
dots f = multi . map f

-- Colours, stroke width, filling... to think about. 



linesUnconnected :: (Fractional u, Ord u, Stroke t) 
                 => t -> [Point2 u] -> Picture u
linesUnconnected t = frameMulti . step where
  step (a:b:xs) = (ostroke t $ vertexPath [a,b]) : step xs
  step _        = []



lineStrip :: (Fractional u, Ord u, Stroke t) 
          => t -> [Point2 u] -> Picture u
lineStrip t = frame . ostroke t . vertexPath

lineLoop :: (Fractional u, Ord u, Stroke t) 
         => t -> [Point2 u] -> Picture u
lineLoop t = frame . cstroke t . vertexPath


triangles :: (Fractional u, Ord u, Stroke t) 
          => t -> [Point2 u] -> Picture u
triangles t = frameMulti . step where
  step (a:b:c:xs) = (cstroke t $ vertexPath [a,b,c]) : step xs
  step _          = []

-- NOTE corners are too sharp...
triangleStrip :: (Fractional u, Ord u, Stroke t) 
              => t -> [Point2 u] -> Picture u
triangleStrip t = frameMulti . step where
  step (a:b:c:xs) = (cstroke t $ vertexPath [a,b,c]) : step (b:c:xs)
  step _          = []

triangleFan :: (Fractional u, Ord u, Stroke t) 
            => t -> [Point2 u] -> Picture u
triangleFan _ []     = error "Wumpus.Extra.Lines.triangleFan - empty list" 
triangleFan t (z:zs) = frameMulti $ step zs where
  step (a:b:xs) = (cstroke t $ vertexPath [z,a,b]) : step (b:xs)
  step _        = []



quads :: (Fractional u, Ord u, Stroke t) 
      => t -> [Point2 u] -> Picture u
quads t = frameMulti . step where
  step (a:b:c:d:xs) = (cstroke t $ vertexPath [a,b,c,d]) : step xs
  step _            = []


quadStrip :: (Fractional u, Ord u, Stroke t) 
          => t -> [Point2 u] -> Picture u
quadStrip t = frameMulti . step where
  step (a:b:c:d:xs) = (cstroke t $ vertexPath [a,b,d,c]) : step (c:d:xs)
  step _            = []
  
  -- Note flipping d & c when creating vertex path,
  -- and only requeuing c & d


--------------------------------------------------------------------------------

-- @bend@ seems most intuitive for \'humps\' - maybe it should 
-- only take one angle...

bend :: (Floating u, Real u, InnerSpace (Vec2 u))
     => Radian -> Radian -> Point2 u -> Point2 u -> CubicBezier u
bend oang iang u v = cubicBezier u a b v
  where
    half_dist = 0.5 * distance u v 
    theta     = langle u v
    a         = u .+^ avec (theta + oang) half_dist 
    b         = v .+^ avec (theta + iang) half_dist


tighten :: Num u => Vec2 u -> Vec2 u -> CubicBezier u -> CubicBezier u
tighten u v (CubicBezier p0 p1 p2 p3) = CubicBezier p0 (p1 .+^ u) (p2 .+^ v) p3
  -- ang = langle p0 p4


-- | Create a tilde (sinusodial) curve about the horizontal plane.
-- 
-- This one is rather simplistic - single one phase curve with no
-- subdivision...
-- 
-- There are better ways to plot things
--
tildeCurve :: (Floating u, AffineSpace (pt u), Converse (Vec2 u)) 
           => u -> Point2 u -> CubicBezier u
tildeCurve w = \pt -> let endpt = pt .+^ hvec w
                      in cubicBezier pt (pt .+^ v) (endpt .+^ converse v) endpt
  where 
    v = avec (pi/4) (w/2)


strline :: Num u => Point2 u -> Vec2 u -> u -> Point2 u
strline pt u t = pt .+^ (t *^ u)

strlineMidpt :: Floating u => Point2 u -> Vec2 u -> Point2 u
strlineMidpt pt u = strline pt u 0.5
