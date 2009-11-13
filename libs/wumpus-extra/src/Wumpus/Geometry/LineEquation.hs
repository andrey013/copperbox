{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.LineEquation
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Lines in equational form, i.e. @Ax + By + C = 0@.
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.LineEquation where

import Wumpus.Geometry.Base

import Wumpus.Core

import Data.AffineSpace

-- | Line in equational form, i.e. @Ax + By + C = 0@.
data Line u = Line !u !u !u 
  deriving (Eq,Show)

-- Would parametric form be more useful 
-- i.e. P2 .+^ (a)V2


--------------------------------------------------------------------------------
-- Instances

instance Functor Line where
  fmap f (Line a b c) = Line (f a) (f b) (f c)


instance (Floating u, Real u) => CCWAngle (Line u) where
  ccwAngle = atan . toRadian . slope


--------------------------------------------------------------------------------
-- Construction

-- Lines are created /without/ respect to frames even though they 
-- are created at arbitrary points. A frame becomes necessary only 
-- later extraction of /points as coordinates/.

line :: Num u => Point2 u -> Point2 u -> Line u
line (P2 x1 y1) (P2 x2 y2) = Line a b c where
    a = y1 - y2
    b = x2 - x1
    c = (x1*y2) - (x2*y1)




-- | Horizontal line goinfg through point @p@.
hline :: Num u => Point2 u -> Line u
hline pt@(P2 x y) = line pt (P2 (-x) y)



-- | Vertical line going through point @p@.
vline :: Num u => Point2 u -> Line u
vline pt@(P2 x y) = line pt (P2 x (-y))




-- | A line in the direction angle @theta@ from x-axis, going 
-- through point @p@.
aline :: Floating u => Radian -> Point2 u -> Line u
aline theta pt = line pt (pt .+^ avec theta 10)





--------------------------------------------------------------------------------
-- operations


slope :: (Fractional u, Real u) => Line u -> u
slope (Line a b _) = (-a)/b 







--------------------------------------------------------------------------------
-- To picture types
