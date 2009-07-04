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

import Wumpus.Core.Line hiding ( line )
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Data.AffineSpace

type Line        = CoLineSegment Point2 Double
type LineBag     = DPoint2 -> [DLineSegment2]  -- unjoined lines


instance Pointwise LineBag where
  type Pt LineBag = DPoint2
  pointwise f pf = pf . f


line :: DVec2 -> Line
line v = \o -> LS o (o .+^ v)

transOrigin :: (Floating a, Pointwise sh, Pt sh ~ Point2 a) 
            => sh -> Point2 a -> sh
transOrigin z = \(P2 x y) -> pointwise (translate x y) z

diamond :: Double -> Double -> DCoPolygon
diamond w h = \o -> Polygon $ map (o .+^) xs 
  where
    xs = [vh,vv, reflectX vh, reflectY vv]
    vh = hvec (w/2)
    vv = vvec (h/2)

--------------------------------------------------------------------------------
-- Dots

dotSquare :: DCoPolygon
dotSquare = square 2

dotPentagon :: DCoPolygon
dotPentagon = regularPolygon 5 2

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


