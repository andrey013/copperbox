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

import Wumpus.Tree.Base

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Anchors                     -- package: wumpus-basic
import Wumpus.Basic.AnchorDots
import Wumpus.Basic.Graphic   
import Wumpus.Basic.Monads.SnocDrawing
import Wumpus.Basic.SVGColours

import Data.VectorSpace                         -- package: vector-space

import Data.Tree

-- Don\'t actually need the Turtle of SnocDrawing...

drawTree :: (a -> TreeNode) -> DrawingAttr -> CoordTree Double a -> DGraphic
drawTree drawF attr tree = 
    execSnocDrawing (regularConfig 1) (0,0) attr
                    $ drawTop drawF tree 


drawTop :: (a -> TreeNode) -> CoordTree Double a -> SnocDrawing Double ()
drawTop drawF (Node (pt,a) ns) = do 
    ancr <- drawF a pt
    mapM_ (draw1 drawF ancr) ns

draw1 :: (a -> TreeNode) 
      -> DotAnchor Double 
      -> CoordTree Double a 
      -> SnocDrawing Double ()
draw1 drawF ancr_from (Node (pt,a) ns) = do
    ancr <- drawF a pt
    connector ancr_from ancr
    mapM_ (draw1 drawF ancr) ns   


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
    

