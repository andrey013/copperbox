{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.BasicCF
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Basic shapes in (a more) coordinate free style
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.BasicCF where

import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Data.AffineSpace

import qualified Wumpus.Drawing.Basic as B

type Line        = DPoint2 -> DLineSegment2
type Polygon     = DPoint2 -> B.Polygon
type LineBag     = DPoint2 -> [DLineSegment2]  -- unjoined lines


instance Pointwise LineBag where
  type Pt LineBag = DPoint2
  pointwise f pf = pf . f

instance Pointwise Line where
  type Pt Line = DPoint2
  pointwise f pf = pf . f

instance Pointwise Polygon where
  type Pt Polygon = DPoint2 
  pointwise f pf = pf . f


line :: DVec2 -> Line
line v = \o -> LS o (o .+^ v)

-- | Draw a regular polgon with @n@ sides, and displacement @vec@ from the
-- center for the first point.
regularPolygon :: Int -> DVec2 -> Polygon
regularPolygon n vec = B.Polygon . points 
  where 
    points (P2 x y) = map (translate x y) $ circular $ replicate n (P2 0 0 .+^ vec) 


diamond :: Double -> Double -> Polygon
diamond w h = \o -> B.Polygon $ map (o .+^) xs 
  where
    xs = [vh,vv, reflectX vh, reflectY vv]
    vh = hvec (w/2)
    vv = vvec (h/2)

--------------------------------------------------------------------------------
-- Dots

dotSquare :: Polygon
dotSquare = regularPolygon 4 (V2 (sqrt 2) (sqrt 2))

dotPentagon :: Polygon 
dotPentagon = regularPolygon 5 (V2 0 2)

dotPlus :: LineBag
dotPlus = sequence [lv,lh]
  where
    lh  = translate (-2) 0 . line (hvec 4)
    lv  = translate 0 (-2) . line (vvec 4)
   

dotX :: LineBag
dotX = sequence [ls1,ls2]
  where
    ls1 o = rotateAbout (pi/6) o $ translate 0 (-2) $ line (vvec 4) o
    ls2 o = rotateAbout (5*pi/3) o $ ls1 o


