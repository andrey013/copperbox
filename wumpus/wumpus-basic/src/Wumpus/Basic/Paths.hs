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



connectS :: Floating u => BPathF u
connectS = \p0 p1 -> execPath p0 $ lineto p1



pathGraphic :: Num u => BPathF u -> DrawingAttr -> GraphicF2 u
pathGraphic pathF attr = \spt ept -> 
    wrapG $ ostroke (strokeAttr attr) $ toPathU $ pathF spt ept



shorten  :: (Real u, Floating u, Ord u) => u -> BPath u -> BPath u
shorten u p = shortenL u $ shortenR u p

--------------------------------------------------------------------------------
-- shorten from the left...

shortenL :: (Real u, Floating u, Ord u) => u -> BPath u -> BPath u
shortenL n (BPath u bp) | n >= u         = emptyPath
                        | otherwise      = step n (viewl bp)
  where
    step _ EmptyL     = emptyPath
    step d (e :< se)  = let z = segmentLength e in
                        case compare d z of
                          GT -> step (d-z) (viewl se)
                          EQ -> BPath (u-n) se
                          _  -> BPath (u-n) (shortenSegL d e <| se)


shortenSegL :: (Real u, Floating u) => u -> BPathSeg u -> BPathSeg u
shortenSegL n (BLineSeg  u l) = BLineSeg  (u-n) (shortenLineL n l) 
shortenSegL n (BCurveSeg u c) = BCurveSeg (u-n) (snd $ subdividet (n/u) c)

shortenLineL :: (Real u, Floating u) => u -> Line u -> Line u
shortenLineL n (Line p1 p2) = Line (p1 .+^ v) p2
  where
    v0 = p2 .-. p1
    v  = avec (direction v0) n
    
--------------------------------------------------------------------------------
-- shorten from the right ...
 
shortenR :: (Real u, Floating u, Ord u) => u -> BPath u -> BPath u
shortenR n (BPath u bp) | n >= u         = emptyPath
                        | otherwise      = step n (viewr bp)
  where
    step _ EmptyR     = emptyPath
    step d (se :> e)  = let z = segmentLength e in
                        case compare d z of
                          GT -> step (d-z) (viewr se)
                          EQ -> BPath (u-n) se
                          _  -> BPath (u-n) (se |> shortenSegR d e)



shortenSegR :: (Real u, Floating u) => u -> BPathSeg u -> BPathSeg u
shortenSegR n (BLineSeg  u l) = BLineSeg  (u-n) (shortenLineR n l) 
shortenSegR n (BCurveSeg u c) = BCurveSeg (u-n) (fst $ subdividet ((u-n)/u) c)


shortenLineR :: (Real u, Floating u) => u -> Line u -> Line u
shortenLineR n (Line p1 p2) = Line p1 (p2 .+^ v)
  where
    v0 = p1 .-. p2
    v  = avec (direction v0) n



--------------------------------------------------------------------------------

midpoint :: (Real u, Floating u) => BPath u -> Point2 u
midpoint (BPath u bp) = step (u/2) (viewl bp)
  where
    step _ EmptyL    = zeroPt
    step d (e :< se) = let z = segmentLength e in
                       case compare d z of
                         GT -> step (d-z) (viewl se)
                         EQ -> segmentEnd e
                         _  -> segmentEnd $ shortenSegL d e 





--------------------------------------------------------------------------------
-- tangents


directionL :: (Real u, Floating u) => BPath u -> Radian
directionL (BPath _ se) = step $ viewl se
  where
    step (BLineSeg  _ l :< _) = lineDirectionL l 
    step (BCurveSeg _ c :< _) = curveDirectionL c
    step _                    = 0

directionR :: (Real u, Floating u) => BPath u -> Radian
directionR (BPath _ se) = step $ viewr se
  where
    step (_ :> BLineSeg  _ l) = lineDirectionR l 
    step (_ :> BCurveSeg _ c) = curveDirectionR c
    step _                    = 0


lineDirectionL :: (Real u, Floating u) => Line u -> Radian
lineDirectionL (Line p0 p1) = direction (pvec p1 p0)

lineDirectionR :: (Real u, Floating u) => Line u -> Radian
lineDirectionR (Line p0 p1) = direction (pvec p0 p1)

curveDirectionL :: (Real u, Floating u) => Curve u -> Radian
curveDirectionL (Curve p0 p1 _ _) = direction $ pvec p1 p0

curveDirectionR :: (Real u, Floating u) => Curve u -> Radian
curveDirectionR (Curve _ _ p2 p3) = direction $ pvec p2 p3
 
