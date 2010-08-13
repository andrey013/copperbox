{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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

    shorten
  , shortenL
  , shortenR

  ) where

import Wumpus.Basic.Paths.Datatypes

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Sequence




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
--



-- | midpoint between two points
--
pointMidpoint :: Fractional u => Point2 u -> Point2 u -> Point2 u
pointMidpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0



--------------------------------------------------------------------------------
-- tangents

startTangent :: (Floating u, Real u) => Curve u -> Radian
startTangent (Curve p0 p1 _ _) = direction $ pvec p0 p1

endTangent :: (Floating u, Real u) => Curve u -> Radian
endTangent (Curve _ _ p2 p3) = direction $ pvec p3 p2
 
endDirection :: (Floating u, Real u) => Curve u -> Radian
endDirection = step . endTangent
  where
    step r | r < pi    = r + pi
           | otherwise = r - pi



incidenceL :: (Real u, Floating u) => Line u -> Radian
incidenceL (Line p1 p2) = direction (pvec p2 p1)

-- incidence on a curve is not the same as the incidence of the
-- respective control point to the end point...
