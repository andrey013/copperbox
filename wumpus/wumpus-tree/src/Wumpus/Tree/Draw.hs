{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Draw
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Version number
--
--------------------------------------------------------------------------------

module Wumpus.Tree.Draw where

import Wumpus.Tree.Algorithm

import Wumpus.Core                      -- package: wumpus-core

import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SVGColours
import Wumpus.Basic.Utils.HList


import Data.Tree

draw :: CoordTree Double a -> DGraphic
draw (Node (pt,_) ns) = dot pt . veloH draw ns . link pt ns


dot :: Fractional u => Point2 u -> Graphic u
dot pt = circle black 4 $ pt

link :: Point2 Double -> [CoordTree Double a] -> DGraphic
link pt ns = veloH step ns
  where
    step (Node (to,_) _) = wrapG $ ostroke black $ vertexPath [pt,to]