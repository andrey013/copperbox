{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Paths.Datatypes
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

module Wumpus.Basic.Paths.Datatypes
  ( 
    BPath(..)
  , BPathSeg(..)
  , Curve(..)
  , Line(..)
  , emptyPath
  , pline
  , pcurve
  , addSegment
  , segmentLength
  , segmentStart
  , segmentEnd

  , toPath 
  , toPathU
  , subdivide
  , subdividet
    
  ) where

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace
import Data.VectorSpace

import Data.Maybe
import Data.Sequence ( Seq, ViewL(..), viewl, (|>)  )
import qualified Data.Sequence as S

-- It would be good not to have a name-clash with Wumpus-Core

data BPath u = BPath 
       { path_length    :: u 
       , path_elements  :: Seq (BPathSeg u)
       }
  deriving (Eq,Ord,Show)


-- Annotation is length...
-- 
data BPathSeg u = BLineSeg  u (Line u)
                | BCurveSeg u (Curve u)
  deriving (Eq,Ord,Show)


data Curve u = Curve 
      { curve_start :: Point2 u
      , ctrl_point1 :: Point2 u
      , ctrl_point2 :: Point2 u
      , curve_end   :: Point2 u
      }
  deriving (Eq,Ord,Show)

data Line u = Line 
      { line_start  :: Point2 u
      , line_end    :: Point2 u 
      }
  deriving (Eq,Ord,Show)


emptyPath :: Num u => BPath u 
emptyPath = BPath 0 S.empty


addSegment :: Num u => BPath u -> BPathSeg u -> BPath u
addSegment (BPath n se) e@(BLineSeg u _)  = BPath (n+u) (se |> e)
addSegment (BPath n se) e@(BCurveSeg u _) = BPath (n+u) (se |> e)

segmentLength :: BPathSeg u -> u
segmentLength (BLineSeg  u _) = u
segmentLength (BCurveSeg u _) = u

segmentStart :: BPathSeg u -> Point2 u
segmentStart (BLineSeg  _ (Line p0 _))      = p0
segmentStart (BCurveSeg _ (Curve p0 _ _ _)) = p0

segmentEnd :: BPathSeg u -> Point2 u
segmentEnd (BLineSeg  _ (Line _ p1))      = p1
segmentEnd (BCurveSeg _ (Curve _ _ _ p3)) = p3




pline :: (Floating u, InnerSpace (Vec2 u)) => Point2 u -> Point2 u -> BPathSeg u 
pline p0 p1 = BLineSeg (distance p0 p1) (Line p0 p1)

pcurve :: (Floating u, Ord u, InnerSpace (Vec2 u))
       => Point2 u -> Point2 u -> Point2 u -> Point2 u -> BPathSeg u 
pcurve p0 p1 p2 p3 = 
    let c = Curve p0 p1 p2 p3 in BCurveSeg (curveLength c) c

-- | Empty path returns Nothing.
--
-- Assumes path is properly formed - i.e. end point of one 
-- segment is the same point as the start point of the next
-- segment.
--
-- TODO - is an empty path actually a problem? ...
--
toPath :: BPath u -> Maybe (Path u)
toPath = step1 . viewl . path_elements
  where
    step1 EmptyL               = Nothing
    step1 (e :< se)            = let (p1,s) = elemP e in 
                                 Just $ path p1 $ s : step2 (viewl se)

    step2 EmptyL               = []
    step2 (e :< se)            = snd (elemP e) : step2 (viewl se)
    
    elemP (BLineSeg  _ l)      = elemL l
    elemP (BCurveSeg _ c)      = elemC c
 
    elemL (Line p1 p2)         = (p1, lineTo p2)
    elemC (Curve p1 p2 p3 p4)  = (p1, curveTo p2 p3 p4)

toPathU :: BPath u -> Path u
toPathU = fromMaybe errK . toPath
  where
    errK = error "toPathU - empty Path"



--------------------------------------------------------------------------------
-- Curve length

curveLength :: (Floating u, Ord u, InnerSpace (Vec2 u))
            => Curve u -> u
curveLength = gravesenLength 0.1

-- | Jens Gravesen\'s bezier arc-length approximation. 
--
-- Note this implementation is parametrized on error tolerance.
--
gravesenLength :: (Floating u, Ord u, InnerSpace (Vec2 u))
               => u -> Curve u -> u
gravesenLength err_tol crv = step crv where
  step c = let l1 = ctrlPolyLength c
               l0 = cordLength c
           in if   l1-l0 > err_tol
              then let (a,b) = subdivide c in step a + step b
              else 0.5*l0 + 0.5*l1


ctrlPolyLength :: (Floating u, InnerSpace (Vec2 u)) 
               => Curve u -> u
ctrlPolyLength (Curve p0 p1 p2 p3) = 
  distance p0 p1 + distance p1 p2 + distance p2 p3

cordLength ::(Floating u, InnerSpace (Vec2 u)) 
           => Curve u -> u
cordLength (Curve p0 _ _ p3) = distance p0 p3


-- | midpoint between two points
--
pointMidpoint :: Fractional u => Point2 u -> Point2 u -> Point2 u
pointMidpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0


-- | Curve subdivision via de Casteljau\'s algorithm.
--
subdivide :: Fractional u => Curve u -> (Curve u, Curve u)
subdivide (Curve p0 p1 p2 p3) =
    (Curve p0 p01 p012 p0123, Curve p0123 p123 p23 p3)
  where
    p01   = pointMidpoint p0    p1
    p12   = pointMidpoint p1    p2
    p23   = pointMidpoint p2    p3
    p012  = pointMidpoint p01   p12
    p123  = pointMidpoint p12   p23
    p0123 = pointMidpoint p012  p123

-- | subdivide with an affine weight along the line...
--
subdividet :: Real u
           => u -> Curve u -> (Curve u, Curve u)
subdividet t (Curve p0 p1 p2 p3) = 
    (Curve p0 p01 p012 p0123, Curve p0123 p123 p23 p3)
  where
    p01   = affineCombination t p0    p1
    p12   = affineCombination t p1    p2
    p23   = affineCombination t p2    p3
    p012  = affineCombination t p01   p12
    p123  = affineCombination t p12   p23
    p0123 = affineCombination t p012  p123

affineCombination :: Real u => u -> Point2 u -> Point2 u -> Point2 u
affineCombination a p1 p2 = p1 .+^ a *^ (p2 .-. p1)

