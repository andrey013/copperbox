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
-- Portability :  GHC with TypeFamilies and more
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

    BPath(..)
  , toPath

  , Curve(..)   -- Temp export
  , Line(..)    -- Temp export
  , lineLength

  ) where

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Sequence ( Seq, ViewL(..), viewl ) 

-- It would be good not to have a name-clash with Wumpus-Core

data BPath u = BPath 
       { path_length    :: u 
       , path_elements  :: Seq (PathSeg u)
       }
  deriving (Eq,Ord,Show)


-- Annotation is length...
data PathSeg u = PLine  u (Line u)
               | PCurve u (Curve u)
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


-- | Empty path returns Nothing.
--
-- Assumes path is properly formed - i.e. end point of one 
-- segment is the same point as the start point of the next
-- segment.
--
toPath :: BPath u -> Maybe (Path u)
toPath = step1 . viewl . path_elements
  where
    step1 EmptyL               = Nothing
    step1 (e :< se)            = let (p1,s) = elemP e in 
                                 Just $ path p1 $ s : step2 (viewl se)

    step2 EmptyL               = []
    step2 (e :< se)            = snd (elemP e) : step2 (viewl se)
    
    elemP (PLine  _ l)          = elemL l
    elemP (PCurve _ c)          = elemC c
 
    elemL (Line p1 p2)         = (p1, lineTo p2)
    elemC (Curve p1 p2 p3 p4)  = (p1, curveTo p2 p3 p4)



lineLength :: (Floating u, InnerSpace (Vec2 u)) => Line u -> u
lineLength (Line p1 p2) = distance p1 p2


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
