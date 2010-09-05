{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Paths
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Extended path type - more amenable for complex drawings than
-- the type in Wumpus-Core.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Paths 
  ( 

    connectS
  , pathGraphic

  , shorten
  , shortenL
  , shortenR
  , midpoint
  , directionL
  , directionR

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Paths.Base
import Wumpus.Basic.Paths.Construction

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Data.Sequence



connectS :: Floating u => PathF u
connectS = \p0 p1 -> execPath p0 $ lineto p1



-- This one might be more useful...

pathGraphic :: Num u => Path u -> DrawingAttr -> Graphic u
pathGraphic bpath attr = 
    wrapG $ ostroke (stroke_colour attr) (stroke_props attr) $ toPrimPathU bpath




shorten  :: (Real u, Floating u, Ord u) => u -> Path u -> Path u
shorten u p = shortenL u $ shortenR u p

--------------------------------------------------------------------------------
-- shorten from the left...

shortenL :: (Real u, Floating u, Ord u) => u -> Path u -> Path u
shortenL n (Path u bp) | n >= u         = emptyPath
                       | otherwise      = step n (viewl bp)
  where
    step _ EmptyL     = emptyPath
    step d (e :< se)  = let z = segmentLength e in
                        case compare d z of
                          GT -> step (d-z) (viewl se)
                          EQ -> Path (u-n) se
                          _  -> Path (u-n) (shortenSegL d e <| se)


shortenSegL :: (Real u, Floating u) => u -> PathSeg u -> PathSeg u
shortenSegL n (LineSeg  u l) = LineSeg  (u-n) (shortenLineL n l) 
shortenSegL n (CurveSeg u c) = CurveSeg (u-n) (snd $ subdividet (n/u) c)

shortenLineL :: (Real u, Floating u) => u -> Line u -> Line u
shortenLineL n (Line p1 p2) = Line (p1 .+^ v) p2
  where
    v0 = p2 .-. p1
    v  = avec (direction v0) n
    
--------------------------------------------------------------------------------
-- shorten from the right ...
 
shortenR :: (Real u, Floating u, Ord u) => u -> Path u -> Path u
shortenR n (Path u bp) | n >= u         = emptyPath
                       | otherwise      = step n (viewr bp)
  where
    step _ EmptyR     = emptyPath
    step d (se :> e)  = let z = segmentLength e in
                        case compare d z of
                          GT -> step (d-z) (viewr se)
                          EQ -> Path (u-n) se
                          _  -> Path (u-n) (se |> shortenSegR d e)



shortenSegR :: (Real u, Floating u) => u -> PathSeg u -> PathSeg u
shortenSegR n (LineSeg  u l) = LineSeg  (u-n) (shortenLineR n l) 
shortenSegR n (CurveSeg u c) = CurveSeg (u-n) (fst $ subdividet ((u-n)/u) c)


shortenLineR :: (Real u, Floating u) => u -> Line u -> Line u
shortenLineR n (Line p1 p2) = Line p1 (p2 .+^ v)
  where
    v0 = p1 .-. p2
    v  = avec (direction v0) n



--------------------------------------------------------------------------------

midpoint :: (Real u, Floating u) => Path u -> Point2 u
midpoint (Path u bp) = step (u/2) (viewl bp)
  where
    step _ EmptyL    = zeroPt
    step d (e :< se) = let z = segmentLength e in
                       case compare d z of
                         GT -> step (d-z) (viewl se)
                         EQ -> segmentEnd e
                         _  -> segmentEnd $ shortenSegL d e 





--------------------------------------------------------------------------------
-- tangents


directionL :: (Real u, Floating u) => Path u -> Radian
directionL (Path _ se) = step $ viewl se
  where
    step (LineSeg  _ l :< _) = lineDirectionL l 
    step (CurveSeg _ c :< _) = curveDirectionL c
    step _                   = 0

directionR :: (Real u, Floating u) => Path u -> Radian
directionR (Path _ se) = step $ viewr se
  where
    step (_ :> LineSeg  _ l) = lineDirectionR l 
    step (_ :> CurveSeg _ c) = curveDirectionR c
    step _                   = 0


lineDirectionL :: (Real u, Floating u) => Line u -> Radian
lineDirectionL (Line p0 p1) = direction (pvec p1 p0)

lineDirectionR :: (Real u, Floating u) => Line u -> Radian
lineDirectionR (Line p0 p1) = direction (pvec p0 p1)

curveDirectionL :: (Real u, Floating u) => Curve u -> Radian
curveDirectionL (Curve p0 p1 _ _) = direction $ pvec p1 p0

curveDirectionR :: (Real u, Floating u) => Curve u -> Radian
curveDirectionR (Curve _ _ p2 p3) = direction $ pvec p2 p3
 
