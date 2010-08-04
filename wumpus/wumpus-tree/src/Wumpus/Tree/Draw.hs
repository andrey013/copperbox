{-# LANGUAGE FlexibleContexts           #-}
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

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Anchors                     -- package: wumpus-basic
import Wumpus.Basic.AnchorDots
import Wumpus.Basic.Graphic   
import Wumpus.Basic.Monads.SnocDrawing
import Wumpus.Basic.SVGColours

import Data.VectorSpace                         -- package: vector-space

import Data.Tree

-- Don\'t actually need the Turtle of SnocDrawing...

drawTree :: CoordTree Double a -> DGraphic
drawTree tree = 
    execSnocDrawing (regularConfig 1) (0,0) (standardAttr 18) $ drawTop tree 


drawTop :: CoordTree Double a -> SnocDrawing Double ()
drawTop (Node (pt,_) ns) = do 
    ancr <- dotCircleEx pt
    mapM_ (draw1 ancr) ns

draw1 :: DotAnchor Double -> CoordTree Double a -> SnocDrawing Double ()
draw1 ancr_from (Node (pt,_) ns) = do
    ancr <- dotCircleEx pt
    connector ancr_from ancr
    mapM_ (draw1 ancr) ns   


connector :: (Floating u, Real u, InnerSpace (Vec2  u)) 
          => DotAnchor u -> DotAnchor u -> SnocDrawing u ()
connector afrom ato = trace1 $ ostroke black $ vertexPath [p0,p1]
   where  
     (ang0,ang1)    = anchorAngles (center afrom) (center ato)
     p0             = radialAnchor ang0 afrom
     p1             = radialAnchor ang1 ato 




anchorAngles :: (Floating u, Real u, InnerSpace (Vec2  u)) 
             => Point2 u -> Point2 u -> (Radian,Radian)
anchorAngles f t = (theta0, theta1)
  where
    conn_v  = pvec f t
    theta0  = direction conn_v
    theta1  = if theta0 < pi then theta0 + pi else theta0 - pi
    

