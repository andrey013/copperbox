{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Test.TypeRestrict
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Type restricted variants of functions with elaborate type signatures.
-- Useful only assert that the original type signature is valid.
-- 
--------------------------------------------------------------------------------

module Wumpus.Test.TypeRestrict where

import Wumpus.Core.Frame
import Wumpus.Core.Line
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.Vector

--------------------------------------------------------------------------------
-- Wumpus.Core.Vector

vangleD :: DVec2 -> DVec2 -> Double 
vangleD = vangle 


-- two vectors in R2 are perpendicular iff their dot product is 0
perpendicularD :: DVec2 -> DVec2 -> Bool
perpendicularD = perpendicular

{-

-- first function just here to test type of second function...
-- note Data.VectorSpace supplies magnitude
 
magnitudeD :: DVec2 -> Double
magnitudeD v@(V2 _ _) = sqrt (v <.> v)

magnitude' :: (InnerSpace (t a), Floating a, a ~ Scalar (t a)) => t a -> a
magnitude' v = sqrt (v <.> v)

-}


--------------------------------------------------------------------------------
-- Wumpus.Core.Frame

piw :: DPoint2 -> Frame2 Double -> DPoint2
piw = pointInWorld




ftofD :: DFrame2 -> DFrame2 -> Matrix3'3 Double
ftofD = ftof


--------------------------------------------------------------------------------
-- Wumpus.Core.Line

segmentLengthD :: DLineSegment2 -> Double
segmentLengthD = segmentLength



-- Points - affine combination

affcombD :: WeightedPoint Rational Point2 Double 
         -> WeightedPoint Rational Point2 Double
         -> Point2 Double
affcombD = affcomb
