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

-- import Wumpus.Core.Instances
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Data.AffineSpace

import qualified Wumpus.Drawing.Basic as B

type Line        = DPoint2 -> DLineSegment2
type Polygon     = DPoint2 -> B.Polygon
type LineBag     = DPoint2 -> [DLineSegment2]  -- unjoined lines


line :: DVec2 -> Line
line v = \o -> LS o (o .+^ v)



diamond :: Double -> Double -> Polygon
diamond w h = \o -> B.Polygon $ map (o .+^) xs 
  where
    xs = [vh,vv, reflectX vh, reflectY vv]
    vh = hvec (w/2)
    vv = vvec (h/2)



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
